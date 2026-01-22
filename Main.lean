import Romanesco.AST
import Romanesco.DynamicParser
import Romanesco.Engine

open Romanesco.AST
open Romanesco.DynamicParser
open Romanesco.Engine

/-- ASTを単純化 (安定版) --/
partial def simplifyAST (t : Term) : Option Term :=
  match t with
  | Term.atom s =>
      let s' := s.trimAscii.toString
      if s'.isEmpty then none else some (Term.atom s')
  | Term.list ts =>
      let ts' := ts.filterMap simplifyAST
      if ts'.isEmpty then none else some (Term.list ts')
  | Term.number n => some (Term.number n)
  | other => some other

partial def skipToNextLine (cs : List Char) : List Char :=
  match cs with | [] => [] | '\n' :: xs => xs | _ :: xs => skipToNextLine xs

/-- 逐次実行ループ --/
partial def runDynamic (env : GrammarEnv) (mEnv : MacroEnv) (rEnv : RuleEnv) (cs : List Char) (lastLen : Nat) : IO Unit :=
  let cs' := skipWs cs
  if cs'.isEmpty then 
    pure ()
  else if cs'.length >= lastLen && lastLen != 0 then do
    IO.println s!"[Fatal] No progress at: '{String.ofList (cs'.take 20)}...'"
    pure ()
  else do
    let results := pRule env (SyntaxRule.nonTerminal "anyDecl") cs' 1000
    if results.isEmpty then do
      IO.println s!"Dynamic parse error at: '{String.ofList (cs'.take 20)}...'"
      runDynamic env mEnv rEnv (skipToNextLine cs') cs'.length
    else do
      let sorted := results.toArray.qsort (λ a b => a.snd.length < b.snd.length)
      let mut solved := false
      for (ast, rest) in sorted do
        if !solved then
          match simplifyAST ast with
          | some (Term.list (Term.atom "syntax" :: Term.atom name :: Term.atom "=" :: ruleAst :: _)) => do
              match compileRule ruleAst with
              | some newRule => do
                  IO.println s!"[Defined Syntax] {name}"
                  solved := true
                  runDynamic ((name, newRule) :: env) mEnv rEnv rest cs'.length
              | none => continue
          | some simpleAst => do
              let solveResults := solve mEnv rEnv (Constraint.fact simpleAst) [] 0
              if !solveResults.isEmpty then do
                IO.println s!"[Solved Statement] {repr simpleAst}"
                solved := true
                runDynamic env mEnv rEnv rest cs'.length
              else continue
          | _ => continue
      
      if !solved then do
        IO.println s!"No valid interpretation found for this block."
        runDynamic env mEnv rEnv (skipToNextLine cs') cs'.length

def main : IO Unit :=
  do
  IO.println "========================================"
  IO.println "   Romanesco Dynamic System v0.5.1      "
  IO.println "========================================"
  let filePath : System.FilePath := "samples/syntax.ro"
  let content ← IO.FS.readFile filePath
  runDynamic bootstrapGrammar [] [] content.toList 0
  IO.println "========================================"