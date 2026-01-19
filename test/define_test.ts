import { evalOmega } from "./helpers/omegaHarness";

async function main() {
  console.log("Testing define...");
  try {
    const result = await evalOmega("(begin (define add1 (lambda (x) (+ x 1))) (add1 5))");
    console.log("Result:", result);
  } catch (e: any) {
    console.log("Error:", e.message);
  }
}
main();
