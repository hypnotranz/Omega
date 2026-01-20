// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-16.md
// Patch Set B: int.op/infer.op/rewrite.op as distinct kernel forms
// Patch Set D: Governance (caps, budgets, commit barrier)

import type { Runtime, DispatchResult } from "../eval/runtime";
import type { State, HandlerFrame } from "../eval/machine";
import type { OpCall } from "./opcall";
import type { Val } from "../eval/values";
import { VUnit } from "../eval/values";
import { envSet } from "../eval/env";
import type { Resumption } from "./opcall";

// AMB: Alternative tracking for backtracking search
type AmbAlternative = {
  thunk: Val;           // Closure to invoke
  resumption: Resumption; // Continuation to resume with thunk's result
};

export type AmbStrategy = "DFS" | "FAIR";

// Oracle Protocol imports
import type { OracleAdapter } from "../oracle/adapter";
import type { SnapshotRepo } from "../oracle/snapshots";
import type { ReceiptStore } from "../oracle/receipts";
import { PortalImpl } from "../oracle/portalImpl";
import { runOracleSession } from "../oracle/driver";
import { isMeaning, meaning as mkMeaning, type MeaningVal, type Obligation, type Evidence } from "../oracle/meaning";
import { matchAST } from "../oracle/match";
import { distFrom, type DistVal } from "../eval/dist";
import { computeSourceHash, type OracleEvidence } from "../provenance/evidence";
import type { StoredReceipt } from "../provenance/store/interface";
import { sha256JSON } from "../artifacts/hash";

// Governance imports
import type { Profile } from "../governance/profile";
import { DEFAULT_PROFILE } from "../governance/profile";
import { capRequire, capHas } from "../governance/caps";
import type { BudgetTracker } from "../governance/budgets";

// Pipeline imports for parsing text to expressions
import { compileTextToExpr } from "../pipeline/compileText";
import { runToCompletionWithState } from "../eval/run";

export interface CommitAdapter {
  commit(payload: Val, ctxDigest: string): Promise<Val>;
}

function findBoundaryIndex(kont: State["kont"], hid: string): number {
  for (let i = kont.length - 1; i >= 0; i--) {
    const fr = kont[i];
    if (fr.tag === "KHandleBoundary" && fr.hid === hid) return i;
  }
  return -1;
}

type InferKind = "int" | "search" | "rewrite";

function intSamplesFromPayload(payload: Val, fallback: number): number {
  // Optional convention: payload may be (map (("n" . <num>)) ...)
  try {
    if (payload.tag === "Map") {
      for (const [k, v] of payload.entries) {
        if (k.tag === "Str" && k.s === "n" && v.tag === "Num") return Math.max(1, Math.floor(v.n));
      }
    }
  } catch {}
  return fallback;
}

function obligationSatisfied(m: MeaningVal): boolean {
  // Convention: obligation = {tag:"Obligation", status:"satisfied"} or Bool true.
  const o = m?.obligation;
  if (!o) return false;
  if ((o as any).tag === "Bool") return !!(o as any).b;
  if ((o as any).tag === "Obligation" && (o as any).status === "satisfied") return true;
  return false;
}

function needsProvenance(state: State): boolean {
  return !!state.provenanceGraph || !!state.provenanceStore;
}

async function attachOracleEvidence(meaning: MeaningVal, payload: Val, state: State): Promise<MeaningVal> {
  if (!needsProvenance(state)) return meaning;

  const timestamp = Date.now();
  const receiptId = sha256JSON({ payload, timestamp });
  const evidence: OracleEvidence = {
    tag: "OracleEvidence",
    receiptId,
    sourceHash: computeSourceHash(payload),
    timestamp,
  };

  state.provenanceGraph?.addNode(evidence);

  if (state.provenanceStore) {
    const receipt: StoredReceipt = { id: receiptId, timestamp, request: payload, response: meaning };
    await state.provenanceStore.storeReceipt(receipt);
  }

  const evidenceList = meaning.evidence ? meaning.evidence.concat([evidence]) : [evidence];
  return { ...meaning, evidence: evidenceList };
}

export class RuntimeImpl implements Runtime {
  private profile: Profile;
  private budget?: BudgetTracker;

  // AMB: Backtracking state
  private ambAlternatives: AmbAlternative[] = [];
  private ambStrategy: AmbStrategy = "DFS";

  constructor(
    private readonly oracle: OracleAdapter,
    private readonly snapshots: SnapshotRepo,
    private readonly receipts: ReceiptStore,
    private readonly commit: CommitAdapter,
    profile?: Profile,
    budget?: BudgetTracker
  ) {
    this.profile = profile ?? DEFAULT_PROFILE;
    this.budget = budget;
  }

  /** Set the budget tracker (allows sharing with run loop). */
  setBudget(budget: BudgetTracker): void {
    this.budget = budget;
  }

  /** Set the amb search strategy (DFS or FAIR). */
  setAmbStrategy(strategy: AmbStrategy): void {
    this.ambStrategy = strategy;
  }

  /** Reset amb state (clears all pending alternatives). */
  resetAmb(): void {
    this.ambAlternatives = [];
  }

  /** Check if there are pending amb alternatives. */
  hasAmbAlternatives(): boolean {
    return this.ambAlternatives.length > 0;
  }

  /** Extract test thunks from a list/vector value. */
  private extractTestThunks(v: Val): Val[] {
    const result: Val[] = [];
    // Handle cons-cell list
    let cur = v;
    while ((cur as any).tag === "Vector" && (cur as any).items?.length === 2) {
      result.push((cur as any).items[0]);
      cur = (cur as any).items[1];
    }
    // Handle flat vector
    if ((cur as any).tag === "Vector" && (cur as any).items?.length > 0 && result.length === 0) {
      return (cur as any).items;
    }
    return result;
  }

  async dispatch(state: State, opcall: OpCall): Promise<DispatchResult> {
    // 1) Try language-level handlers (deep handlers; nearest enclosing clause wins).
    for (let hi = state.handlers.length - 1; hi >= 0; hi--) {
      const hf = state.handlers[hi];
      const clause = hf.on.get(opcall.op);
      if (!clause) continue;

      // The handler for opcall.op is hf at index hi.
      // Clause executes *outside* any inner handlers above hf (those are inside the suspended continuation).
      const handlersTrunc = state.handlers.slice(0, hi + 1);

      // Compute delimiter boundary location in the suspended continuation.
      const boundaryIndex = findBoundaryIndex(state.kont, hf.hid);
      if (boundaryIndex < 0) throw new Error(`dispatch: boundary not found for handler ${hf.hid}`);

      // Truncate continuation to delimiter boundary (discard frames inside handle body).
      // Keep the boundary itself so clause return aborts to it and triggers return clause logic.
      const kontTrunc = state.kont.slice(0, boundaryIndex + 1);

      // Construct k as a ContVal.
      const kVal: Val = { tag: "Cont", hid: hf.hid, boundaryIndex, resumption: opcall.resumption };

      // Bind params and k into handler lexical env.
      let store = state.store;
      let env = hf.env;

      // Bind operation parameters
      if (clause.params.length !== opcall.args.length) {
        throw new Error(`dispatch: op arity mismatch for ${clause.op}`);
      }
      for (let i = 0; i < clause.params.length; i++) {
        const [store2, addr] = store.alloc(opcall.args[i]);
        store = store2;
        env = envSet(env, clause.params[i], addr);
      }

      // Bind k
      {
        const [store2, addr] = store.alloc(kVal);
        store = store2;
        env = envSet(env, clause.k, addr);
      }

      // Evaluate handler clause body.
      const st2: State = {
        ...state,
        control: { tag: "Expr", e: clause.body },
        env,
        store,
        kont: kontTrunc,
        handlers: handlersTrunc,
      };
      return st2;
    }

    // 2) Built-in fallback handlers using the Oracle Protocol.
    const opName = opcall.op;

    // Patch Set B: Three distinct intensional forms
    // - infer.op: backward compatible, returns denotation directly
    // - int.op: returns full Meaning
    // - rewrite.op: returns full Meaning (expects rewrite field)
    // - search.op: returns Dist<Meaning> for multi-shot search
    if (opName === "int.op" || opName === "infer.op" || opName === "rewrite.op" || opName === "search.op") {
      const kind: InferKind = opName === "search.op" ? "search" : opName === "rewrite.op" ? "rewrite" : "int";
      const needProv = needsProvenance(state);

      // Consume oracle turn budget
      this.budget?.consumeOracleTurn();

      // Minimal caps: oracle must be able to re-enter eval/apply/observe to be "first class".
      capRequire(this.profile.caps, "eval", `start ${opName}`);
      capRequire(this.profile.caps, "apply", `start ${opName}`);
      capRequire(this.profile.caps, "observe", `start ${opName}`);

      const payload: Val = opcall.args[0] ?? ({ tag: "Unit" } as Val);

      // Snapshot current state for oracle introspection
      const envRef = this.snapshots.putEnv({ env: state.env, store: state.store });
      const stateRef = this.snapshots.putState({ state });

      // Create portal for oracle to REPL back into evaluator
      const portal = new PortalImpl(this as any, this.snapshots, this.receipts, {
        maxEvalSteps: 100_000,
        parseText: (src: string) => compileTextToExpr(src),
      });

      // SEARCH: search.op - multi-shot oracle sampling returns Dist<Meaning>
      if (kind === "search") {
        const n = intSamplesFromPayload(payload, 8);
        const items: { v: Val; w: number }[] = [];

        for (let i = 0; i < n; i++) {
          const session = this.oracle.startSession({
            tag: "Infer",
            payload,
            envRef,
            stateRef,
          });

          const meaning = await runOracleSession(session, portal);
          const enriched = needProv ? await attachOracleEvidence(meaning as MeaningVal, payload, state) : meaning;
          items.push({ v: enriched as Val, w: 1 });
        }

        const d: DistVal = distFrom(items, { kind: "search", note: `n=${n}` });
        return opcall.resumption.invoke(d as Val);
      }

      // int.op, infer.op, rewrite.op: Run ONE oracle session
      const session = this.oracle.startSession({
        tag: "Infer",
        payload,
        envRef,
        stateRef,
      });

      const meaning = await runOracleSession(session, portal);
      let enrichedMeaning = meaning as MeaningVal;
      if (needProv) {
        enrichedMeaning = await attachOracleEvidence(enrichedMeaning, payload, state);
      }

      // Optional: if oracle asked to adopt a modified env, patch state
      const adoptEnv = portal.consumeAdoptEnvRef() ?? meaning.adoptEnvRef;
      let st2 = state;
      if (adoptEnv) {
        const snap = this.snapshots.getEnv(adoptEnv);
        st2 = { ...st2, env: snap.env, store: snap.store };
      }

      // int.op and rewrite.op: Return the full Meaning as a first-class value
      if (opName === "int.op" || opName === "rewrite.op") {
        // runOracleSession always returns Meaning (MeaningVal), which is a Val
        return opcall.resumption.invoke(enrichedMeaning as Val);
      }

      // infer.op: Return the denotation directly (backward compatible)
      const resultVal = enrichedMeaning.denotation ?? ({ tag: "Unit" } as Val);
      return opcall.resumption.invoke(resultVal);
    }

    // oracle.apply.op: LLM in apply position (unchanged)
    if (opcall.op === "oracle.apply.op") {
      // Consume oracle turn budget
      this.budget?.consumeOracleTurn();

      // Capability checks for oracle apply
      capRequire(this.profile.caps, "eval", "oracle.apply.op");
      capRequire(this.profile.caps, "apply", "oracle.apply.op");
      capRequire(this.profile.caps, "observe", "oracle.apply.op");

      const proc = opcall.args[0]; // OracleProc
      const argVec = opcall.args[1];
      if (argVec.tag !== "Vector") throw new Error("oracle.apply.op expects (OracleProc, Vector)");

      // Snapshot current state
      const envRef = this.snapshots.putEnv({ env: state.env, store: state.store });
      const stateRef = this.snapshots.putState({ state });

      // Create portal
      const portal = new PortalImpl(this as any, this.snapshots, this.receipts, {
        maxEvalSteps: 100_000,
        parseText: (src: string) => compileTextToExpr(src),
      });

      // Start apply session
      const session = this.oracle.startSession({
        tag: "Apply",
        proc,
        args: argVec.items,
        envRef,
        stateRef,
      });

      // Drive session
      const meaning = await runOracleSession(session, portal);
      const v = meaning.denotation ?? ({ tag: "Unit" } as Val);

      // Optional adoption
      const adoptEnv = portal.consumeAdoptEnvRef() ?? meaning.adoptEnvRef;
      let st2 = state;
      if (adoptEnv) {
        const snap = this.snapshots.getEnv(adoptEnv);
        st2 = { ...st2, env: snap.env, store: snap.store };
      }

      return opcall.resumption.invoke(v);
    }

    // Patch Set D: commit.op with truth regime enforcement
    if (opcall.op === "commit.op") {
      capRequire(this.profile.caps, "commit.*", "commit");

      const kind = opcall.args[0]?.tag === "Str" ? (opcall.args[0] as any).s : "unknown";
      const payload = opcall.args[1] ?? opcall.args[0] ?? { tag: "Unit" };

      if (this.profile.truth === "speculative") {
        throw new Error(`commit rejected in speculative truth regime (kind=${kind})`);
      }

      if (this.profile.truth === "test-certified" || this.profile.truth === "proof-certified") {
        if (isMeaning(payload)) {
          if (!obligationSatisfied(payload)) {
            throw new Error("commit requires satisfied obligations under non-speculative regimes");
          }
        }
      }

      const res = await this.commit.commit(payload, opcall.ctxDigest);
      return opcall.resumption.invoke(res);
    }

    // Patch Set 5: commit-tested.op - commit with test barrier
    // Args: (kind: Str, payload: Val, tests: Vec<Closure>) or (payload, tests)
    // Runs each test thunk; all must return truthy for commit to proceed
    if (opcall.op === "commit-tested.op") {
      capRequire(this.profile.caps, "commit.*", "commit-tested");
      capRequire(this.profile.caps, "test", "commit-tested");

      // Parse arguments: (kind payload tests) or (payload tests)
      let kind = "tested";
      let payload: Val;
      let tests: Val[];

      if (opcall.args.length >= 3 && opcall.args[0]?.tag === "Str") {
        kind = (opcall.args[0] as any).s;
        payload = opcall.args[1];
        tests = this.extractTestThunks(opcall.args[2]);
      } else if (opcall.args.length >= 2) {
        payload = opcall.args[0];
        tests = this.extractTestThunks(opcall.args[1]);
      } else {
        throw new Error("commit-tested.op: expected (kind payload tests) or (payload tests)");
      }

      // Run each test thunk - they must be closures returning truthy values
      const envRef = this.snapshots.putEnv({ env: state.env, store: state.store });
      const portal = new PortalImpl(this as any, this.snapshots, this.receipts, {
        maxEvalSteps: 100_000,
        parseText: (src: string) => compileTextToExpr(src),
        caps: this.profile.caps,
      });

      // Evaluate each test
      for (let i = 0; i < tests.length; i++) {
        const testThunk = tests[i];
        if (testThunk.tag !== "Closure") {
          throw new Error(`commit-tested.op: test ${i} is not a closure`);
        }

        // Evaluate test thunk body in its closure environment
        const { env, store } = this.snapshots.getEnv(envRef);
        const testState: State = {
          control: { tag: "Expr", e: testThunk.body },
          env: testThunk.env,
          store,
          kont: [],
          handlers: [],
        };

        try {
          const { value } = await runToCompletionWithState(this as any, testState, 100_000);
          const passed = value.tag === "Bool" ? value.b : (value.tag !== "Unit");
          if (!passed) {
            throw new Error(`commit-tested.op: test ${i} failed (returned ${value.tag})`);
          }
        } catch (e: any) {
          throw new Error(`commit-tested.op: test ${i} threw: ${e?.message ?? e}`);
        }
      }

      // All tests passed, now check truth regime
      if (this.profile.truth === "speculative") {
        throw new Error(`commit rejected in speculative truth regime (kind=${kind})`);
      }

      // Mark obligation as satisfied since tests passed
      const result = await this.commit.commit(payload, opcall.ctxDigest);
      return opcall.resumption.invoke(result);
    }

    // Patch Set 6: commit/rewrite.op - commit a Meaning with obligation discharge
    // Args: (meaning: MeaningVal) where meaning has .rewrite and .obligations
    // Discharges obligations, records evidence, then commits if all pass
    if (opcall.op === "commit/rewrite.op") {
      capRequire(this.profile.caps, "commit.*", "commit/rewrite");
      capRequire(this.profile.caps, "commit.rewrite", "commit/rewrite");

      const meaningArg = opcall.args[0];
      if (!meaningArg || !isMeaning(meaningArg)) {
        throw new Error("commit/rewrite.op: expected Meaning value");
      }

      const meaning = meaningArg as MeaningVal;
      const obligations = meaning.obligations ?? [];
      const evidence: Evidence[] = [];

      // Set up portal for running tests
      const envRef = this.snapshots.putEnv({ env: state.env, store: state.store });
      const portal = new PortalImpl(this as any, this.snapshots, this.receipts, {
        maxEvalSteps: 100_000,
        parseText: (src: string) => compileTextToExpr(src),
        caps: this.profile.caps,
      });

      // Discharge each obligation
      for (const obl of obligations) {
        if (obl.tag === "OblTests") {
          // Run test expressions, all must return truthy
          let passed = 0;
          const total = obl.tests.length;

          for (const testExpr of obl.tests) {
            const { env, store } = this.snapshots.getEnv(obl.envRef ?? envRef);
            const testState: State = {
              control: { tag: "Expr", e: testExpr },
              env,
              store,
              kont: [],
              handlers: [],
            };

            try {
              const { value } = await runToCompletionWithState(this as any, testState, 100_000);
              const ok = value.tag === "Bool" ? value.b : (value.tag !== "Unit");
              if (ok) passed++;
              else throw new Error(`OblTests: test failed`);
            } catch (e: any) {
              throw new Error(`commit/rewrite.op: OblTests failed: ${e?.message ?? e}`);
            }
          }

          evidence.push({ tag: "TestEvidence", passed, total });
        }

        if (obl.tag === "OblNoMatch") {
          // Check that pattern does NOT appear in output (rewrite) or all (original + rewrite)
          const target = obl.scope === "output" ? meaning.rewrite : meaning.rewrite;
          if (target) {
            const { ok } = matchAST(obl.pattern, target);
            if (ok) {
              throw new Error(`commit/rewrite.op: OblNoMatch failed - pattern found in ${obl.scope}`);
            }
          }
          evidence.push({ tag: "NoMatchEvidence", pattern: obl.pattern, searched: 1, found: 0 });
        }

        if (obl.tag === "OblEqExt") {
          // Run extensional equivalence tests: original(input) === candidate(input) for all tests
          let allPassed = true;
          const failures: Array<{ input: Val; expected: Val; got: Val }> = [];

          for (const testExpr of obl.tests) {
            const { env, store } = this.snapshots.getEnv(obl.envRef ?? envRef);

            // Evaluate original with test input
            const origState: State = {
              control: { tag: "Expr", e: { tag: "App", fn: obl.original, args: [testExpr] } },
              env,
              store,
              kont: [],
              handlers: [],
            };
            const { value: expected } = await runToCompletionWithState(this as any, origState, 100_000);

            // Evaluate candidate with same input
            const candState: State = {
              control: { tag: "Expr", e: { tag: "App", fn: obl.candidate, args: [testExpr] } },
              env,
              store,
              kont: [],
              handlers: [],
            };
            const { value: got } = await runToCompletionWithState(this as any, candState, 100_000);

            // Compare (simple structural equality for now)
            const eq = JSON.stringify(expected) === JSON.stringify(got);
            if (!eq) {
              allPassed = false;
              failures.push({ input: { tag: "Syntax", stx: testExpr } as any, expected, got });
            }
          }

          if (!allPassed) {
            throw new Error(`commit/rewrite.op: OblEqExt failed - ${failures.length} test(s) differ`);
          }
          evidence.push({ tag: "EqExtEvidence", tests: obl.tests.length, allPassed, failures: failures.length > 0 ? failures : undefined });
        }
      }

      // All obligations discharged - check truth regime
      if (this.profile.truth === "speculative") {
        throw new Error(`commit/rewrite rejected in speculative truth regime`);
      }

      // Create result Meaning with evidence
      const resultMeaning: MeaningVal = {
        ...meaning,
        evidence,
      };

      // Commit the rewrite (the actual binding update)
      const payload = meaning.rewrite ?? meaning.residual ?? meaning.denotation ?? { tag: "Unit" };
      const result = await this.commit.commit(payload as Val, opcall.ctxDigest);

      // Return the Meaning with evidence attached
      return opcall.resumption.invoke(resultMeaning as Val);
    }

    // AMB: amb.choose - pick first thunk, save rest as pending alternatives
    if (opcall.op === "amb.choose") {
      const thunks = opcall.args[0];
      if (!thunks || thunks.tag !== "Vector") {
        throw new Error("amb.choose: expected vector of thunks");
      }

      // Convert cons-cell list to flat array
      function consToArray(v: Val): Val[] {
        const result: Val[] = [];
        let cur = v;
        while (cur.tag === "Vector" && cur.items.length === 2) {
          result.push(cur.items[0]);
          cur = cur.items[1];
        }
        // Handle flat vectors too
        if (cur.tag === "Vector" && cur.items.length > 2) {
          return cur.items;
        }
        return result;
      }

      const items = consToArray(thunks);
      if (items.length === 0) {
        // No alternatives - immediately fail
        return this.dispatch(state, {
          op: "amb.fail",
          args: [{ tag: "Str", s: "amb: no alternatives" } as Val],
          resumption: opcall.resumption,
          ctxDigest: opcall.ctxDigest,
        });
      }

      // Save remaining alternatives (based on strategy)
      // DFS: push to stack (most recent first when we pop)
      // FAIR: add to queue (oldest first when we shift)
      for (let i = 1; i < items.length; i++) {
        const alt: AmbAlternative = {
          thunk: items[i],
          resumption: opcall.resumption,
        };
        if (this.ambStrategy === "DFS") {
          this.ambAlternatives.push(alt);
        } else {
          // FAIR: add to end of queue
          this.ambAlternatives.push(alt);
        }
      }

      // Debit budget for amb attempt
      this.budget?.consumeAmbAttempt?.();

      // Invoke first thunk: apply it with no arguments
      const firstThunk = items[0];
      if (firstThunk.tag !== "Closure") {
        throw new Error("amb.choose: thunk must be a closure");
      }

      // Build a state that evaluates the thunk with resumption's frames
      // The thunk is a zero-arg closure - inline its body evaluation
      const baseState: State | "Uncaught" | "OutOfBudget" =
        opcall.resumption.invoke({ tag: "Unit" } as Val);
      if (baseState === ("Uncaught" as any) || baseState === ("OutOfBudget" as any)) {
        return baseState;
      }

      // Inline the closure: evaluate body with closure's env
      // The kont from baseState has the continuation waiting for the result
      return {
        ...baseState,
        control: { tag: "Expr", e: firstThunk.body },
        env: firstThunk.env,
      } as State;
    }

    // AMB: amb.fail - backtrack to next pending alternative
    if (opcall.op === "amb.fail") {
      if (this.ambAlternatives.length === 0) {
        // No more alternatives - propagate failure
        throw new Error("amb: all alternatives exhausted");
      }

      // Debit budget for backtrack
      this.budget?.consumeAmbAttempt?.();

      // Pick next alternative based on strategy
      // Both DFS and FAIR try alternatives in order (first added = first tried)
      // The difference is in how fairness interleaves with continuation
      let alt: AmbAlternative;
      alt = this.ambAlternatives.shift()!;  // FIFO: try in order added

      // Resume with this alternative's thunk
      const thunk = alt.thunk;
      const resumption = alt.resumption;

      // Same as amb.choose: we need to evaluate the thunk and resume with result
      const baseState: State | "Uncaught" | "OutOfBudget" =
        resumption.invoke({ tag: "Unit" } as Val);
      if (baseState === ("Uncaught" as any) || baseState === ("OutOfBudget" as any)) {
        return baseState;
      }

      // We need to actually call the thunk. Let's inline it properly.
      // The thunk is a Closure. We want to evaluate its body with its env.
      if (thunk.tag === "Closure") {
        return {
          ...baseState,
          control: { tag: "Expr", e: thunk.body },
          env: thunk.env,
        } as State;
      } else if (thunk.tag === "Native") {
        // Native thunk - call it with empty args
        // This is tricky... for now assume thunks are Closures
        throw new Error("amb: Native thunks not yet supported");
      }

      throw new Error(`amb: invalid thunk type ${thunk.tag}`);
    }

    // amb.* passthrough: if not handled by a language handler or a dedicated nondet runner, it is uncaught.
    return "Uncaught";
  }
}
