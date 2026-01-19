import { evalOmega } from "./helpers/omegaHarness";

async function main() {
  const result = await evalOmega(`
    (begin
      (define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))))
      (factorial 5))
  `);
  console.log("factorial(5) =", result);
}
main().catch(e => console.error("Error:", e.message));
