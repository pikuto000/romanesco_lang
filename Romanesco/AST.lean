namespace Romanesco.AST

/-- 項 --/
inductive Term
  | atom : String → Term
  | number : Nat → Term
  | var : String → Term
  | compound : String → List Term → Term
  | list : List Term → Term
  deriving Repr, BEq, Inhabited

/-- 制約 --/
inductive Constraint
  | equal : Term → Term → Constraint
  | conj : Constraint → Constraint → Constraint
  | disj : Constraint → Constraint → Constraint
  | fact : Term → Constraint
  | call : String → List Term → Constraint
  deriving Repr, BEq, Inhabited

/-- 構文定義のためのメタデータ --/
inductive SyntaxRule
  | terminal : String → SyntaxRule       -- 固定文字列 (例: "if")
  | nonTerminal : String → SyntaxRule    -- 他のルール参照 (例: expression)
  | seq : List SyntaxRule → SyntaxRule   -- 連続 (A B C)
  | alt : List SyntaxRule → SyntaxRule   -- 選択 (A | B)
  | many : SyntaxRule → SyntaxRule       -- 繰り返し (A*)
  deriving Repr, BEq, Inhabited

/-- 宣言 --/
inductive Declaration
  | macroDef : String → List String → Term → Declaration
  | ruleDef : String → List String → Constraint → Declaration -- 述語定義
  | syntaxDef : String → SyntaxRule → Declaration             -- 構文定義 (例: def syntax expr = ...)
  | action : Constraint → Declaration
  deriving Repr, Inhabited

end Romanesco.AST