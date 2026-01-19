// Quick REPL test
import { evalOmega } from "./helpers/omegaHarness";

async function main() {
  console.log("Testing OmegaLLM REPL API...\n");

  // Basic arithmetic
  console.log("(+ 1 2 3) =>", await evalOmega("(+ 1 2 3)"));
  console.log("(- 10 3) =>", await evalOmega("(- 10 3)"));
  console.log("(= 5 5) =>", await evalOmega("(= 5 5)"));
  console.log("(= 5 6) =>", await evalOmega("(= 5 6)"));

  // Lambda and application
  console.log("((lambda (x) (+ x 1)) 5) =>", await evalOmega("((lambda (x) (+ x 1)) 5)"));

  // Let binding
  console.log("(let ((x 10)) (+ x 5)) =>", await evalOmega("(let ((x 10)) (+ x 5))"));

  // Nested let
  console.log("(let ((x 2)) (let ((y 3)) (+ x y))) =>", await evalOmega("(let ((x 2)) (let ((y 3)) (+ x y)))"));

  // If expression
  console.log("(if (= 1 1) 42 0) =>", await evalOmega("(if (= 1 1) 42 0)"));

  console.log("\n✓ REPL API works!");
}

main().catch(console.error);
