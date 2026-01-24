namespace Romanesco.AST

/-- 項 --/
inductive Term
  | atom : String → Term
  | number : Nat → Term
  | var : String → Term
  | compound : String → List Term → Term
  | list : List Term → Term
  deriving Repr, BEq, Inhabited, Hashable

/-- 制約 --/
inductive Constraint
  | equal : Term → Term → Constraint
  | conj : Constraint → Constraint → Constraint
  | disj : Constraint → Constraint → Constraint
  | fact : Term → Constraint
  | call : String → List Term → Constraint
  deriving Repr, BEq, Inhabited, Hashable

/-- 字句・構文の役割カテゴリ --/
inductive LexicalCategory
  | prefix    -- 接頭詞 (!x)
  | suffix    -- 接尾詞 (x?)
  | infix     -- 中置 (x + y)
  | unary     -- 単項 (Standalone)
  | delimiter -- 括弧ペア ( (x) )
  | separator -- 区切り文字 (x, y)
  | keyword   -- 予約語 (syntax)
  | comment   -- コメント (# ...)
  deriving Repr, BEq, Inhabited, Hashable

/-- 構文定義のためのメタデータ --/
inductive SyntaxRule
  | terminal : String → SyntaxRule
  | nonTerminal : String → SyntaxRule
  | seq : List SyntaxRule → SyntaxRule
  | alt : List SyntaxRule → SyntaxRule
  | many : SyntaxRule → SyntaxRule
  | many1 : SyntaxRule → SyntaxRule
  | anyChar : SyntaxRule
  | charRange : Char → Char → SyntaxRule
  | categorized : LexicalCategory → SyntaxRule → SyntaxRule -- カテゴリ付きルール
  deriving Repr, BEq, Inhabited, Hashable

/-- 宣言 --/
inductive Declaration
  | macroDef : String → List String → Term → Declaration
  | ruleDef : String → List String → Constraint → Declaration
  | syntaxDef : String → SyntaxRule → Declaration
  | action : Constraint → Declaration
  deriving Repr, Inhabited

end Romanesco.AST
