import { evalOmega } from "./helpers/omegaHarness";
async function main() {
  // Test simple let
  console.log("let test:", await evalOmega("(let ((x 5)) (* x x))"));
  
  // Test non-recursive define
  console.log("define test:", await evalOmega("(begin (define x 42) x)"));
  
  // Test define with lambda (non-recursive)
  console.log("define lambda:", await evalOmega("(begin (define add1 (lambda (x) (+ x 1))) (add1 5))"));
}
main().catch(e => console.error("Error:", e.message));
