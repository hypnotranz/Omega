// SOURCE: ARCHITECTURE/32-LANGUAGE-OFFICIAL-13-IMPLEMENTATION-11.md
// AUTO-EXTRACTED - Do not edit directly. Edit the source document.

export type Expr =
  | { tag: "Lit"; value: number | string | boolean | null }
  | { tag: "Var"; name: string }
  | { tag: "Lambda"; params: string[]; body: Expr }
  | { tag: "OracleLambda"; params: string[]; spec: Expr }  // oracle-lambda: first-class inference procedure
  | { tag: "If"; test: Expr; conseq: Expr; alt: Expr }
  | { tag: "Begin"; exprs: Expr[] }
  | { tag: "Define"; name: string; rhs: Expr }
  | { tag: "Set"; name: string; rhs: Expr }
  | { tag: "App"; fn: Expr; args: Expr[] }
  | { tag: "Quote"; datum: unknown }
  | { tag: "Effect"; op: string; args: Expr[] }
  | { tag: "Handle"; body: Expr; handler: HandlerExpr }
  | { tag: "Match"; scrutinee: Expr; clauses: Array<{ pat: Pattern; body: Expr }> };

export type Pattern =
  | { tag: "PWild" }
  | { tag: "PVar"; name: string } // binder name (already internalized by lowering)
  | { tag: "PLit"; value: number | string | boolean | null }
  | { tag: "PVector"; items: Pattern[] };

export type HandlerExpr = {
  on: Array<{ op: string; params: string[]; k: string; body: Expr }>;
  ret?: { v: string; body: Expr };
  fin?: { body: Expr };
};