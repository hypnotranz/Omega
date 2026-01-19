// src/core/oracle/scriptedOracle.ts
// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-14.md
// This is the test oracle that proves your plane works without calling any model.

import type { OracleAdapter, OracleInit } from "./adapter";
import type { OracleSession, OracleResp, Meaning } from "./protocol";
import type { Expr } from "../ast";
import type { Val } from "../eval/values";

function exprPlus(a: number, b: number): Expr {
  return {
    tag: "App",
    fn: { tag: "Var", name: "+" },
    args: [{ tag: "Lit", value: a }, { tag: "Lit", value: b }],
  } as any;
}

/** Extract a string from a Val for dispatch purposes */
function getSpecKey(spec: Val): string {
  if (spec.tag === "Str") return spec.s;
  if (spec.tag === "Sym") return spec.name;
  if (spec.tag === "Vector" && spec.items.length > 0 && spec.items[0].tag === "Sym") {
    return spec.items[0].name;
  }
  return "default";
}

export class ScriptedOracleAdapter implements OracleAdapter {
  startSession(init: OracleInit): OracleSession {
    if (init.tag === "Infer") return this.inferSession(init.payload, init.envRef, init.stateRef);
    return this.applySession(init.proc, init.args, init.envRef, init.stateRef);
  }

  private inferSession(_payload: Val, envRef: string, stateRef: string): OracleSession {
    return (async function* (): OracleSession {
      // 1) Observe stack (prove we can introspect the paused state)
      let r: OracleResp = yield { tag: "ReqObserve", what: { tag: "Stack", limit: 8 }, stateRef };
      const stackDepth = r.tag === "RespObs" ? String((r.data as unknown[])?.length ?? 0) : "unknown";

      // 2) Ask runtime to evaluate (+ 20 22)
      r = yield { tag: "ReqEval", qexpr: exprPlus(20, 22), envRef };
      if (r.tag !== "RespVal") {
        return { tag: "Meaning", confidence: 0, trace: { tag: "Str", s: `error: eval failed, stackDepth=${stackDepth}` } };
      }

      const sum = r.value;

      // 3) Return a Meaning whose denotation is that value
      const meaning: Meaning = {
        tag: "Meaning",
        denotation: sum,
        confidence: 1.0,
        trace: { tag: "Str", s: `stackDepth=${stackDepth}` },
      };

      yield { tag: "ReqReturn", result: meaning };
      return meaning;
    })();
  }

  private applySession(proc: Val, args: Val[], envRef: string, stateRef: string): OracleSession {
    // Extract spec from OracleProc to dispatch behavior
    const spec = proc.tag === "OracleProc" ? proc.spec : { tag: "Str", s: "default" } as Val;
    const specKey = getSpecKey(spec);

    // Dispatch based on spec
    switch (specKey) {
      case "apply-add1":
        return this.applyAdd1Session(args);

      case "apply-add":
        return this.applyAddSession(args);

      case "must-reenter-eval":
        return this.reenterEvalSession(args, envRef);

      case "repair-loop":
        return this.repairLoopSession(args, envRef, stateRef);

      // B4: Return full Meaning value
      case "return-meaning":
        return this.returnMeaningSession(args);

      // B7: Attempt a commit (will fail in speculative regime)
      case "do-commit":
        return this.doCommitSession(args);

      // B8: Return Meaning with satisfied obligation
      case "return-certified-meaning":
        return this.returnCertifiedMeaningSession(args);

      // ─────────────────────────────────────────────────────────────────
      // Prompt 3: Semantic tests
      // ─────────────────────────────────────────────────────────────────

      // 3.1: Classify sentiment - returns 'positive or 'negative symbol
      case "classify-sentiment":
        return this.classifySentimentSession(args);

      // 3.2: Extract named entity - returns string with entity name
      case "extract-named-entity":
        return this.extractNamedEntitySession(args);

      // 3.3: Semantic rewrite - uses ReqEval to call string-replace-all
      case "semantic-rewrite":
        return this.semanticRewriteSession(args, envRef);

      // 3.4: Match AST shape - returns result of pattern matching
      case "match-ast-shape":
        return this.matchAstShapeSession(args);

      // 3.5: Get tree accessor - returns accessor for ADT
      case "get-tree-accessor":
        return this.getTreeAccessorSession(args, envRef);

      // 3.6: Suggest number - for Dist search, returns candidate number
      case "suggest-number":
        return this.suggestNumberSession(args);

      // 3.6: Check safety predicate
      case "check-safety":
        return this.checkSafetySession(args);

      // ─────────────────────────────────────────────────────────────────
      // Prompt 4: AMB and Streams semantic tests
      // ─────────────────────────────────────────────────────────────────

      // 4.1: find-sensitive - returns sensitivity level of data
      case "find-sensitive":
        return this.findSensitiveSession(args);

      // 4.1: safe? - checks if data is safe to process
      case "safe?":
        return this.safePredicateSession(args);

      // 4.1: semantic-redact - redacts sensitive info from text
      case "semantic-redact":
        return this.semanticRedactSession(args);

      // 4.2: classify-ticket - classifies support ticket to department
      case "classify-ticket":
        return this.classifyTicketSession(args);

      // 4.4: rank-strategy - returns priority ranking for a strategy
      case "rank-strategy":
        return this.rankStrategySession(args);

      // 4.5: classify-content - classifies content type
      case "classify-content":
        return this.classifyContentSession(args);

      // 4.6: expensive-compute - simulates expensive computation (tracks calls)
      case "expensive-compute":
        return this.expensiveComputeSession(args);

      // 4.7: safe-predicate - checks safety for stream filtering
      case "safe-predicate":
        return this.safePredicateSession(args);

      // ─────────────────────────────────────────────────────────────────
      // Prompt 5: Generic Operations semantic tests
      // ─────────────────────────────────────────────────────────────────

      // 5.3: synthesize-processor - synthesizes a processor type for unknown data
      case "synthesize-processor":
        return this.synthesizeProcessorSession(args);

      default:
        // Default: add args using runtime
        return this.defaultApplySession(args, envRef);
    }
  }

  /** C1 Test: Simple add1 - directly return x + 1 */
  private applyAdd1Session(args: Val[]): OracleSession {
    return (async function* (): OracleSession {
      const x = args[0]?.tag === "Num" ? args[0].n : 0;
      const m: Meaning = { tag: "Meaning", denotation: { tag: "Num", n: x + 1 }, confidence: 1 };
      yield { tag: "ReqReturn", result: m };
      return m;
    })();
  }

  /** C3 Test: Simple add - directly return x + y */
  private applyAddSession(args: Val[]): OracleSession {
    return (async function* (): OracleSession {
      const x = args[0]?.tag === "Num" ? args[0].n : 0;
      const y = args[1]?.tag === "Num" ? args[1].n : 0;
      const m: Meaning = { tag: "Meaning", denotation: { tag: "Num", n: x + y }, confidence: 1 };
      yield { tag: "ReqReturn", result: m };
      return m;
    })();
  }

  /** C4 Test: Re-entrant eval - use ReqEval and ReqApply */
  private reenterEvalSession(args: Val[], envRef: string): OracleSession {
    return (async function* (): OracleSession {
      // 1) ReqEval to get the '+' function
      let r: OracleResp = yield { tag: "ReqEval", qexpr: { tag: "Var", name: "+" } as Expr, envRef };
      if (r.tag !== "RespVal") {
        const m: Meaning = { tag: "Meaning", confidence: 0, trace: { tag: "Str", s: `error: eval failed` } };
        return m;
      }

      const plusFn = r.value;

      // 2) ReqApply to apply '+' to [x, 1]
      const x = args[0] ?? { tag: "Num", n: 0 };
      r = yield { tag: "ReqApply", fn: plusFn, args: [x, { tag: "Num", n: 1 }], envRef };
      if (r.tag !== "RespVal") {
        const m: Meaning = { tag: "Meaning", confidence: 0, trace: { tag: "Str", s: `error: apply failed` } };
        return m;
      }

      const m: Meaning = {
        tag: "Meaning",
        denotation: r.value,
        confidence: 1,
        trace: { tag: "Str", s: "used ReqEval + ReqApply" },
      };
      yield { tag: "ReqReturn", result: m };
      return m;
    })();
  }

  /** C5 Test: Multi-shot rollback via ReqSnapshot/ReqHydrate */
  private repairLoopSession(args: Val[], envRef: string, stateRef: string): OracleSession {
    return (async function* (): OracleSession {
      // 1) Take a snapshot
      let r: OracleResp = yield { tag: "ReqSnapshot", envRef, stateRef };
      const receipt = r.tag === "RespObs" ? (r.data as { receipt?: string })?.receipt ?? "R0" : "R0";

      // 2) Try a wrong computation (would return wrong result)
      // In a real scenario, the oracle would detect an error and hydrate back
      // For testing, we simulate: first attempt wrong, then correct

      // 3) Hydrate back to the snapshot
      r = yield { tag: "ReqHydrate", receiptId: receipt };

      // 4) Now compute correctly: x + 1
      const x = args[0]?.tag === "Num" ? args[0].n : 0;
      const m: Meaning = {
        tag: "Meaning",
        denotation: { tag: "Num", n: x + 1 },
        confidence: 1,
        trace: { tag: "Str", s: `used snapshot/hydrate loop, receipt=${receipt}` },
      };
      yield { tag: "ReqReturn", result: m };
      return m;
    })();
  }

  /** Default: use runtime to add args */
  private defaultApplySession(args: Val[], envRef: string): OracleSession {
    return (async function* (): OracleSession {
      // Demonstrate: oracle can get a function and apply it by asking runtime.
      // First, eval '+' to obtain the primitive.
      let r: OracleResp = yield { tag: "ReqEval", qexpr: { tag: "Var", name: "+" } as Expr, envRef };
      if (r.tag !== "RespVal") {
        const m: Meaning = { tag: "Meaning", confidence: 0, trace: { tag: "Str", s: `error: eval failed tag=${r.tag}` } };
        yield { tag: "ReqReturn", result: m };
        return m;
      }

      const plusFn = r.value;

      // Now request apply of '+' to args
      r = yield { tag: "ReqApply", fn: plusFn, args, envRef };
      if (r.tag !== "RespVal") {
        const m: Meaning = { tag: "Meaning", confidence: 0, trace: { tag: "Str", s: `error: apply failed tag=${r.tag}` } };
        yield { tag: "ReqReturn", result: m };
        return m;
      }

      const m: Meaning = { tag: "Meaning", denotation: r.value, confidence: 1 };
      yield { tag: "ReqReturn", result: m };
      return m;
    })();
  }

  /** B4 Test: Return full Meaning value */
  private returnMeaningSession(args: Val[]): OracleSession {
    return (async function* (): OracleSession {
      const x = args[0]?.tag === "Num" ? args[0].n : 0;
      const m: Meaning = {
        tag: "Meaning",
        denotation: { tag: "Num", n: x + 1 },
        confidence: 1,
        trace: { tag: "Str", s: "return-meaning session" },
      };
      yield { tag: "ReqReturn", result: m };
      return m;
    })();
  }

  /** B7 Test: Return a meaning without satisfied obligations (for commit rejection test) */
  private doCommitSession(args: Val[]): OracleSession {
    return (async function* (): OracleSession {
      // Returns a meaning WITHOUT satisfied obligation
      // When this is passed to commit.op in test-certified regime, it should fail
      const payload = args[0] ?? { tag: "Unit" };
      const m: Meaning = {
        tag: "Meaning",
        denotation: payload,
        confidence: 1,
        // No obligation field = not certified
        trace: { tag: "Str", s: "do-commit session without obligation" },
      };
      yield { tag: "ReqReturn", result: m };
      return m;
    })();
  }

  /** B8 Test: Return Meaning with satisfied obligation */
  private returnCertifiedMeaningSession(args: Val[]): OracleSession {
    return (async function* (): OracleSession {
      const x = args[0]?.tag === "Num" ? args[0].n : 0;
      const m: Meaning = {
        tag: "Meaning",
        denotation: { tag: "Num", n: x + 1 },
        confidence: 1,
        obligation: { tag: "Obligation", status: "satisfied" },
        trace: { tag: "Str", s: "certified meaning with satisfied obligation" },
      };
      yield { tag: "ReqReturn", result: m };
      return m;
    })();
  }

  // ─────────────────────────────────────────────────────────────────
  // Prompt 3: Semantic test oracle methods
  // ─────────────────────────────────────────────────────────────────

  /** 3.1: Classify sentiment - simulates LLM sentiment classification
   * Input: string text
   * Output: 'positive or 'negative symbol
   */
  private classifySentimentSession(args: Val[]): OracleSession {
    return (async function* (): OracleSession {
      const text = args[0]?.tag === "Str" ? args[0].s : "";

      // Simulate sentiment classification based on keywords
      const lowerText = text.toLowerCase();
      const positiveWords = ["good", "great", "excellent", "happy", "love", "wonderful", "amazing", "best"];
      const negativeWords = ["bad", "terrible", "awful", "sad", "hate", "horrible", "worst", "poor"];

      let positiveScore = 0;
      let negativeScore = 0;

      for (const word of positiveWords) {
        if (lowerText.includes(word)) positiveScore++;
      }
      for (const word of negativeWords) {
        if (lowerText.includes(word)) negativeScore++;
      }

      const sentiment = positiveScore >= negativeScore ? "positive" : "negative";
      const m: Meaning = {
        tag: "Meaning",
        denotation: { tag: "Sym", name: sentiment },
        confidence: 1,
        trace: { tag: "Str", s: `classified "${text.substring(0, 20)}..." as ${sentiment}` },
      };
      yield { tag: "ReqReturn", result: m };
      return m;
    })();
  }

  /** 3.2: Extract named entity - simulates NER extraction
   * Input: string text
   * Output: string with extracted entity name (or "UNKNOWN")
   */
  private extractNamedEntitySession(args: Val[]): OracleSession {
    return (async function* (): OracleSession {
      const text = args[0]?.tag === "Str" ? args[0].s : "";

      // Simple entity extraction: find capitalized words
      // In real LLM, this would be semantic understanding
      const words = text.split(/\s+/);
      let entity = "UNKNOWN";

      for (const word of words) {
        // Find capitalized word that's not at sentence start
        if (word.length > 1 && word[0] === word[0].toUpperCase() && word[0] !== word[0].toLowerCase()) {
          // Skip common sentence starters
          if (!["The", "A", "An", "This", "That", "It", "I"].includes(word)) {
            entity = word.replace(/[.,!?;:]/g, ""); // Remove punctuation
            break;
          }
        }
      }

      const m: Meaning = {
        tag: "Meaning",
        denotation: { tag: "Str", s: entity },
        confidence: entity === "UNKNOWN" ? 0.5 : 1.0,
        trace: { tag: "Str", s: `extracted entity "${entity}" from text` },
      };
      yield { tag: "ReqReturn", result: m };
      return m;
    })();
  }

  /** 3.3: Semantic rewrite - uses ReqEval to call string-replace-all
   * Demonstrates hybrid approach: oracle decides WHAT to rewrite,
   * then uses REPL to do the mechanical rewrite
   * Input: (text entity replacement)
   * Output: rewritten text
   */
  private semanticRewriteSession(args: Val[], envRef: string): OracleSession {
    return (async function* (): OracleSession {
      const text = args[0]?.tag === "Str" ? args[0].s : "";
      const entity = args[1]?.tag === "Str" ? args[1].s : "";
      const replacement = args[2]?.tag === "Str" ? args[2].s : "";

      // Use ReqEval to call string-replace-all from the runtime
      // This demonstrates the oracle using mechanical tools
      const replaceExpr: Expr = {
        tag: "App",
        fn: { tag: "Var", name: "string-replace-all" },
        args: [
          { tag: "Lit", value: text },
          { tag: "Lit", value: entity },
          { tag: "Lit", value: replacement },
        ],
      } as any;

      const r: OracleResp = yield { tag: "ReqEval", qexpr: replaceExpr, envRef };

      if (r.tag !== "RespVal") {
        const m: Meaning = {
          tag: "Meaning",
          denotation: { tag: "Str", s: text }, // Fall back to original
          confidence: 0,
          trace: { tag: "Str", s: "semantic-rewrite: ReqEval failed" },
        };
        return m;
      }

      const m: Meaning = {
        tag: "Meaning",
        denotation: r.value,
        confidence: 1,
        trace: { tag: "Str", s: `rewrote "${entity}" to "${replacement}" using ReqEval` },
      };
      yield { tag: "ReqReturn", result: m };
      return m;
    })();
  }

  /** 3.4: Match AST shape - simulates pattern matching on AST
   * Input: AST represented as quoted list structure (cons-cell or flat)
   * Output: matched pattern variables or #f
   */
  private matchAstShapeSession(args: Val[]): OracleSession {
    return (async function* (): OracleSession {
      const ast = args[0];

      // Helper to convert cons-cell list to flat array
      function listToArray(v: Val): Val[] | null {
        if (v.tag !== "Vector") return null;
        const result: Val[] = [];
        let cur: Val = v;
        // Handle both flat vectors and cons-cell lists
        while (cur.tag === "Vector") {
          if (cur.items.length === 2) {
            // Cons cell: [head, tail]
            result.push(cur.items[0]);
            cur = cur.items[1];
          } else if (cur.items.length > 0) {
            // Flat vector: return all items
            return cur.items;
          } else {
            break;
          }
        }
        return result.length > 0 ? result : null;
      }

      // Check if AST matches pattern (if (eq? ?x 0) 1 ...)
      // For testing, we just check if it's a conditional
      function isConditional(v: Val): boolean {
        const arr = listToArray(v);
        if (!arr || arr.length < 2) return false;
        const head = arr[0];
        if (head.tag === "Sym" && head.name === "if") return true;
        return false;
      }

      function extractBindings(v: Val): Val {
        // Extract pattern variable bindings as a list
        // For (if (eq? x 0) 1 body), extract x and body
        const arr = listToArray(v);
        if (!arr || arr.length < 4) {
          return { tag: "Bool", b: false };
        }

        const condition = arr[1];
        const thenBranch = arr[2];
        const elseBranch = arr[3];

        // Build bindings list: ((condition . <cond>) (then . <then>) (else . <else>))
        const bindings: Val = {
          tag: "Vector",
          items: [
            {
              tag: "Vector",
              items: [{ tag: "Sym", name: "condition" }, condition]
            },
            {
              tag: "Vector",
              items: [
                {
                  tag: "Vector",
                  items: [{ tag: "Sym", name: "then" }, thenBranch]
                },
                {
                  tag: "Vector",
                  items: [
                    {
                      tag: "Vector",
                      items: [{ tag: "Sym", name: "else" }, elseBranch]
                    },
                    { tag: "Unit" }
                  ]
                }
              ]
            }
          ]
        };
        return bindings;
      }

      const matched = isConditional(ast);
      const result = matched ? extractBindings(ast) : { tag: "Bool", b: false } as Val;

      const m: Meaning = {
        tag: "Meaning",
        denotation: result,
        confidence: matched ? 1 : 0,
        trace: { tag: "Str", s: matched ? "matched conditional pattern" : "pattern did not match" },
      };
      yield { tag: "ReqReturn", result: m };
      return m;
    })();
  }

  /** 3.5: Get tree accessor - returns appropriate accessor for ADT representation
   * Input: representation tag ('list-tree or 'record-tree)
   * Output: accessor functions wrapped in a list
   */
  private getTreeAccessorSession(args: Val[], envRef: string): OracleSession {
    return (async function* (): OracleSession {
      const repTag = args[0]?.tag === "Sym" ? args[0].name : "list-tree";

      // Return accessor functions based on representation
      // For list-tree: (value left right) = (car car.cdr car.cdr.cdr)
      // For record-tree: use get-field accessors

      let accessors: Val;

      if (repTag === "list-tree") {
        // Use ReqEval to get the standard accessors
        let r: OracleResp = yield { tag: "ReqEval", qexpr: { tag: "Var", name: "car" } as Expr, envRef };
        const carFn = r.tag === "RespVal" ? r.value : { tag: "Unit" } as Val;

        r = yield { tag: "ReqEval", qexpr: { tag: "Var", name: "cadr" } as Expr, envRef };
        const cadrFn = r.tag === "RespVal" ? r.value : { tag: "Unit" } as Val;

        r = yield { tag: "ReqEval", qexpr: { tag: "Var", name: "caddr" } as Expr, envRef };
        const caddrFn = r.tag === "RespVal" ? r.value : { tag: "Unit" } as Val;

        // Return (list value-accessor left-accessor right-accessor)
        accessors = {
          tag: "Vector",
          items: [
            carFn,
            { tag: "Vector", items: [cadrFn, { tag: "Vector", items: [caddrFn, { tag: "Unit" }] }] }
          ]
        };
      } else {
        // For record-tree, return symbolic accessor names
        accessors = {
          tag: "Vector",
          items: [
            { tag: "Sym", name: "tree-value" },
            {
              tag: "Vector",
              items: [
                { tag: "Sym", name: "tree-left" },
                { tag: "Vector", items: [{ tag: "Sym", name: "tree-right" }, { tag: "Unit" }] }
              ]
            }
          ]
        };
      }

      const m: Meaning = {
        tag: "Meaning",
        denotation: accessors,
        confidence: 1,
        trace: { tag: "Str", s: `returned accessors for ${repTag}` },
      };
      yield { tag: "ReqReturn", result: m };
      return m;
    })();
  }

  /** 3.6: Suggest number - returns a candidate number for Dist search
   * Input: constraints (min, max)
   * Output: suggested number
   */
  private suggestNumberSession(args: Val[]): OracleSession {
    return (async function* (): OracleSession {
      const min = args[0]?.tag === "Num" ? args[0].n : 0;
      const max = args[1]?.tag === "Num" ? args[1].n : 100;

      // Suggest a number in range (deterministic for testing)
      const suggested = Math.floor((min + max) / 2);

      const m: Meaning = {
        tag: "Meaning",
        denotation: { tag: "Num", n: suggested },
        confidence: 1,
        trace: { tag: "Str", s: `suggested ${suggested} in range [${min}, ${max}]` },
      };
      yield { tag: "ReqReturn", result: m };
      return m;
    })();
  }

  /** 3.6: Check safety predicate - determines if operation is "safe"
   * Input: operation description
   * Output: #t or #f
   */
  private checkSafetySession(args: Val[]): OracleSession {
    return (async function* (): OracleSession {
      const op = args[0];

      // Check if operation looks "safe"
      // For testing: numbers > 0 are safe, strings without "danger" are safe
      let isSafe = true;

      if (op.tag === "Num") {
        isSafe = op.n > 0;
      } else if (op.tag === "Str") {
        isSafe = !op.s.toLowerCase().includes("danger");
      } else if (op.tag === "Sym") {
        isSafe = !op.name.toLowerCase().includes("danger");
      }

      const m: Meaning = {
        tag: "Meaning",
        denotation: { tag: "Bool", b: isSafe },
        confidence: 1,
        trace: { tag: "Str", s: `safety check: ${isSafe ? "safe" : "unsafe"}` },
      };
      yield { tag: "ReqReturn", result: m };
      return m;
    })();
  }

  // ─────────────────────────────────────────────────────────────────
  // Prompt 4: AMB and Streams semantic test oracle methods
  // ─────────────────────────────────────────────────────────────────

  /** 4.1: find-sensitive - returns sensitivity level of data
   * Input: string data
   * Output: 'high, 'medium, 'low, or 'none symbol
   */
  private findSensitiveSession(args: Val[]): OracleSession {
    return (async function* (): OracleSession {
      const data = args[0]?.tag === "Str" ? args[0].s : "";
      const lowerData = data.toLowerCase();

      let level: string;
      if (lowerData.includes("ssn") || lowerData.includes("password") || lowerData.includes("credit card")) {
        level = "high";
      } else if (lowerData.includes("email") || lowerData.includes("phone") || lowerData.includes("address")) {
        level = "medium";
      } else if (lowerData.includes("name") || lowerData.includes("id")) {
        level = "low";
      } else {
        level = "none";
      }

      const m: Meaning = {
        tag: "Meaning",
        denotation: { tag: "Sym", name: level },
        confidence: 1,
        trace: { tag: "Str", s: `sensitivity level: ${level}` },
      };
      yield { tag: "ReqReturn", result: m };
      return m;
    })();
  }

  /** 4.1/4.7: safe? - checks if data is safe to process
   * Input: any value
   * Output: #t or #f
   */
  private safePredicateSession(args: Val[]): OracleSession {
    return (async function* (): OracleSession {
      const data = args[0];
      let isSafe = true;

      if (data.tag === "Str") {
        const lowerData = data.s.toLowerCase();
        isSafe = !lowerData.includes("danger") && !lowerData.includes("unsafe") && !lowerData.includes("malicious");
      } else if (data.tag === "Num") {
        isSafe = data.n >= 0 && data.n < 1000; // Numbers in safe range
      } else if (data.tag === "Sym") {
        isSafe = !data.name.toLowerCase().includes("danger");
      }

      const m: Meaning = {
        tag: "Meaning",
        denotation: { tag: "Bool", b: isSafe },
        confidence: 1,
        trace: { tag: "Str", s: `safe? check: ${isSafe}` },
      };
      yield { tag: "ReqReturn", result: m };
      return m;
    })();
  }

  /** 4.1: semantic-redact - redacts sensitive info from text
   * Input: string text, symbol sensitivity-level
   * Output: redacted text string
   */
  private semanticRedactSession(args: Val[]): OracleSession {
    return (async function* (): OracleSession {
      const text = args[0]?.tag === "Str" ? args[0].s : "";
      const level = args[1]?.tag === "Sym" ? args[1].name : "low";

      let redacted = text;

      // Redact based on sensitivity level
      if (level === "high" || level === "medium" || level === "low") {
        // Redact SSN-like patterns
        redacted = redacted.replace(/\d{3}-\d{2}-\d{4}/g, "[REDACTED-SSN]");
        // Redact email-like patterns
        redacted = redacted.replace(/[\w.-]+@[\w.-]+\.\w+/g, "[REDACTED-EMAIL]");
        // Redact phone-like patterns
        redacted = redacted.replace(/\d{3}[-.]?\d{3}[-.]?\d{4}/g, "[REDACTED-PHONE]");
      }

      const m: Meaning = {
        tag: "Meaning",
        denotation: { tag: "Str", s: redacted },
        confidence: 1,
        trace: { tag: "Str", s: `redacted at level ${level}` },
      };
      yield { tag: "ReqReturn", result: m };
      return m;
    })();
  }

  /** 4.2: classify-ticket - classifies support ticket to department
   * Input: ticket description string
   * Output: symbol ('billing, 'technical, 'sales, 'general)
   */
  private classifyTicketSession(args: Val[]): OracleSession {
    return (async function* (): OracleSession {
      const ticket = args[0]?.tag === "Str" ? args[0].s : "";
      const lowerTicket = ticket.toLowerCase();

      let department: string;
      if (lowerTicket.includes("bill") || lowerTicket.includes("payment") || lowerTicket.includes("invoice") || lowerTicket.includes("charge")) {
        department = "billing";
      } else if (lowerTicket.includes("bug") || lowerTicket.includes("error") || lowerTicket.includes("crash") || lowerTicket.includes("not working")) {
        department = "technical";
      } else if (lowerTicket.includes("buy") || lowerTicket.includes("price") || lowerTicket.includes("discount") || lowerTicket.includes("purchase")) {
        department = "sales";
      } else {
        department = "general";
      }

      const m: Meaning = {
        tag: "Meaning",
        denotation: { tag: "Sym", name: department },
        confidence: 1,
        trace: { tag: "Str", s: `classified ticket to ${department}` },
      };
      yield { tag: "ReqReturn", result: m };
      return m;
    })();
  }

  /** 4.4: rank-strategy - returns priority ranking for a strategy
   * Input: strategy symbol
   * Output: number (lower is higher priority)
   */
  private rankStrategySession(args: Val[]): OracleSession {
    return (async function* (): OracleSession {
      const strategy = args[0]?.tag === "Sym" ? args[0].name : "unknown";

      // Priority ranking: cache > local > remote > fallback
      const rankings: Record<string, number> = {
        "cache": 1,
        "local": 2,
        "fast": 2,
        "remote": 3,
        "slow": 3,
        "fallback": 4,
        "default": 5,
      };

      const rank = rankings[strategy.toLowerCase()] ?? 10;

      const m: Meaning = {
        tag: "Meaning",
        denotation: { tag: "Num", n: rank },
        confidence: 1,
        trace: { tag: "Str", s: `strategy ${strategy} ranked ${rank}` },
      };
      yield { tag: "ReqReturn", result: m };
      return m;
    })();
  }

  /** 4.5: classify-content - classifies content type
   * Input: content string
   * Output: symbol ('code, 'text, 'data, 'mixed)
   */
  private classifyContentSession(args: Val[]): OracleSession {
    return (async function* (): OracleSession {
      const content = args[0]?.tag === "Str" ? args[0].s : "";

      let contentType: string;
      // Check for code indicators
      if (content.includes("{") || content.includes("}") || content.includes("function") || content.includes("=>") || content.includes("def ")) {
        contentType = "code";
      } else if (content.includes("[") && content.includes("]") || content.includes(",") && /\d+/.test(content)) {
        contentType = "data";
      } else if (content.length > 50 && content.split(" ").length > 10) {
        contentType = "text";
      } else {
        contentType = "mixed";
      }

      const m: Meaning = {
        tag: "Meaning",
        denotation: { tag: "Sym", name: contentType },
        confidence: 1,
        trace: { tag: "Str", s: `classified content as ${contentType}` },
      };
      yield { tag: "ReqReturn", result: m };
      return m;
    })();
  }

  // Call counter for expensive-compute (for memoization testing)
  private static expensiveComputeCallCount = 0;

  /** 4.6: expensive-compute - simulates expensive computation
   * Tracks call count to verify memoization works
   * Input: any value
   * Output: computed result + increments counter
   */
  private expensiveComputeSession(args: Val[]): OracleSession {
    const self = this.constructor as typeof ScriptedOracleAdapter;
    return (async function* (): OracleSession {
      self.expensiveComputeCallCount++;
      const input = args[0];

      // Return input * 2 for numbers, "COMPUTED:" prefix for strings
      let result: Val;
      if (input.tag === "Num") {
        result = { tag: "Num", n: input.n * 2 };
      } else if (input.tag === "Str") {
        result = { tag: "Str", s: `COMPUTED:${input.s}` };
      } else {
        result = { tag: "Str", s: "COMPUTED:unknown" };
      }

      const m: Meaning = {
        tag: "Meaning",
        denotation: result,
        confidence: 1,
        trace: { tag: "Str", s: `expensive-compute call #${self.expensiveComputeCallCount}` },
      };
      yield { tag: "ReqReturn", result: m };
      return m;
    })();
  }

  /** Reset the expensive-compute call counter (for test setup) */
  static resetExpensiveComputeCounter(): void {
    ScriptedOracleAdapter.expensiveComputeCallCount = 0;
  }

  /** Get the expensive-compute call count (for test assertions) */
  static getExpensiveComputeCallCount(): number {
    return ScriptedOracleAdapter.expensiveComputeCallCount;
  }

  // ─────────────────────────────────────────────────────────────────
  // Prompt 5: Generic Operations
  // ─────────────────────────────────────────────────────────────────

  /** 5.3: synthesize-processor - synthesizes a processor for unknown data type
   * Input: type-tag symbol, data value
   * Output: symbol indicating the processor type to use
   */
  private synthesizeProcessorSession(args: Val[]): OracleSession {
    return (async function* (): OracleSession {
      const typeTag = args[0]?.tag === "Sym" ? args[0].name : "unknown";
      // const data = args[1]; // Could inspect data if needed

      // Synthesize a processor based on the type tag
      let processor: string;
      switch (typeTag) {
        case "markdown":
        case "html":
        case "text":
        case "document":
          processor = "text-processor";
          break;
        case "image":
        case "binary":
        case "media":
          processor = "binary-processor";
          break;
        case "json":
        case "xml":
        case "data":
          processor = "data-processor";
          break;
        default:
          processor = "generic-processor";
          break;
      }

      const m: Meaning = {
        tag: "Meaning",
        denotation: { tag: "Sym", name: processor },
        confidence: 0.8, // Lower confidence for synthesized processors
        trace: { tag: "Str", s: `synthesized ${processor} for type ${typeTag}` },
      };
      yield { tag: "ReqReturn", result: m };
      return m;
    })();
  }
}
