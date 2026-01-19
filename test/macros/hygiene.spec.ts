// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-4.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

import { describe, it, expect } from "vitest";

// Assumed helper you implement once your reader/expander/lowerer is wired:
//   evalOmega(sourceText) -> JS number/string/bool
import { evalOmega } from "../helpers/omegaHarness";

describe("macro hygiene (capture avoidance)", () => {
  it("H1: introduced temp should not capture user binding", async () => {
    const src = `
      (begin
        (define-syntax m
          (syntax-rules ()
            ((_ x) (let ((t 10)) x))))
        (let ((t 99))
          (m t)))
    `;
    const v = await evalOmega(src);
    expect(v).toBe(99);
  });

  it("H2: user binding should not capture introduced temp referenced by macro", async () => {
    const src = `
      (begin
        (define-syntax m
          (syntax-rules ()
            ((_ x)
              (let ((t 10))
                (+ t x)))))
        (let ((t 99))
          (m 1)))
    `;
    const v = await evalOmega(src);
    // Must be 11, not 100. Macro's internal t must refer to its own t=10.
    expect(v).toBe(11);
  });
});