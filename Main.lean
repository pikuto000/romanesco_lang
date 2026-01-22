import Romanesco.AST
import Romanesco.DynamicParser

open Romanesco.AST
open Romanesco.DynamicParser

partial def runBootstrap (env : GrammarEnv) (cs : List Char) : IO Unit := do
  if cs.isEmpty then return
  
  -- 宣言をパース
  let results := pRule env (SyntaxRule.nonTerminal "decl") cs
  if results.isEmpty then
    IO.println s!"Parse error at: '{String.ofList (cs.take 20)}...'"
    return
  
  -- 最長一致を選択
  let sorted := results.toArray.qsort (fun a b => a.2.length < b.2.length)
  let (ast, rest) := sorted[0]!
  
  match ast with
  | Term.list [Term.atom "syntax", _, Term.atom name, _, Term.atom "=", _, ruleBody] => do
      -- 構文定義の場合
      IO.println s!"[Syntax] {name} defined."
      -- 簡易的なコンパイル (実際には ruleBody の解析が必要)
      runBootstrap ((name, SyntaxRule.terminal "placeholder") :: env) rest
  | Term.atom _ => do
      -- 空白やコメントの場合（無視して進む）
      runBootstrap env rest
  | _ => do
      IO.println s!"[Parsed] {repr ast}"
      runBootstrap env rest

def main : IO Unit := do
  IO.println "--- Romanesco Pure Parser (Including Whitespace) ---"
  let filePath := "samples/syntax.ro"
  let content ← IO.FS.readFile filePath
  
  runBootstrap bootstrapGrammar content.toList
