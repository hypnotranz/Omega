import { evalOmega } from "./helpers/omegaHarness";

async function test() {
  // Recursive sum: sum(n) = n + (n-1) + ... + 1
  const result = await evalOmega(`
    (let ((sum (lambda (f n)
                 (if (= n 0)
                     0
                     (+ n (f f (- n 1)))))))
      (sum sum 5))
  `);
  console.log("sum(5) = 5+4+3+2+1 =>", result, "(expected: 15)");
}
test();
