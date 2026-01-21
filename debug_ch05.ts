import { createRuntime, registerCommonOracleScripts, valToNative } from "./demo/by-chapter/shared";
import { chapterConfigs } from "./demo/by-chapter/config";
import { createScriptedOracleAdapter } from "./demo/harness/oracle-adapter";
import { createDemoLedger } from "./demo/harness/ledger";
import { createSeededRandom, getProfile } from "./demo/harness/runner";
import type { DemoContext } from "./demo/harness/types";

const config = chapterConfigs["ch05-nondeterministic"];
const seed = 7;
const profile = getProfile("pragmatic");
const oracle = createScriptedOracleAdapter(config.id, seed, "pragmatic");
const ledger = createDemoLedger();
const random = createSeededRandom(seed);
const ctx: DemoContext = { profile, seed, random, oracle, ledger, isReplay: false, options: {} };
(oracle as any).setContext?.(ctx);
config.setupOracle?.(ctx);
registerCommonOracleScripts(ctx);

const omega = createRuntime(ctx);
(async () => {
  const program = config.programs[0];
  const evalResult = await omega.eval(`(begin ${program.code})`);
  console.log('eval ok', evalResult.ok);
  if (evalResult.ok) {
    console.log('native', valToNative(evalResult.value as any));
  } else {
    console.log('error', evalResult.error);
  }
})();
