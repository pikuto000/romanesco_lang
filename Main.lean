import Romanesco.AST
import Romanesco.DynamicParser
import Romanesco.Engine
import Lean.Data.Options

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

def traceRuntime (opts : Lean.Options) (msg : Unit → String) : IO Unit :=
  if opts.getBool `trace.romanesco.runtime then
    IO.println s!"trace[romanesco.runtime] {msg ()}"
  else
    pure ()

/-- 逐次実行ループ --/
partial def runDynamic (opts : Lean.Options) (env : GrammarEnv) (mEnv : MacroEnv) (rEnv : RuleEnv) (cs : List Char) (lastLen : Nat) : IO Unit :=
  let cs' := skipWs cs
  if cs'.isEmpty then 
    traceRuntime opts fun _ => "Finished."
    pure ()
  else if cs'.length >= lastLen && lastLen != 0 then do
    IO.println s!"[Fatal] No progress at: '{String.ofList (cs'.take 20)}...'"
    traceRuntime opts fun _ => "Stuck. Check parser traces if enabled."
    pure ()
  else do
    traceRuntime opts fun _ => s!"Parsing at offset {lastLen - cs'.length} (remaining: {cs'.length})"
    let results := pRule opts env (SyntaxRule.nonTerminal "anyDecl") cs' 1000
    if results.isEmpty then do
      IO.println s!"Dynamic parse error at: '{String.ofList (cs'.take 20)}...'"
      traceRuntime opts fun _ => "Parser returned no results. skipping to next line..."
      runDynamic opts env mEnv rEnv (skipToNextLine cs') cs'.length
    else do
      let sorted := results.toArray.qsort (λ a b => a.snd.length < b.snd.length)
      let mut solved := false
      traceRuntime opts fun _ => s!"Parser found {sorted.size} candidates."
      
      for (rawAst, rest) in sorted do
        if !solved then
          -- 構文木を正規化 (name [body] -> compound name body)
          let ast := normalizeParsed rawAst
          match ast with
          | Term.compound "anyDecl" [Term.compound "syntaxDef" [_, _, Term.compound "B_IDENT" [Term.atom name], _, _, _, ruleAst]] => do
              match compileRule ruleAst with
              | some newRule => do
                  IO.println s!"[Defined Syntax] {name}"
                  traceRuntime opts fun _ => s!"Compiled rule for {name}: {repr newRule}"
                  solved := true
                  runDynamic opts ((name, newRule) :: env) mEnv rEnv rest cs'.length
              | none => 
                  traceRuntime opts fun _ => s!"Failed to compile rule for {name}"
                  pure ()
          | Term.compound "anyDecl" [Term.compound "ruleDef" [_, _, Term.compound "B_IDENT" [Term.atom headName], _, _, args, _, _, _, bodyAst]] => do
              -- 引数リストから変数名を抽出
              let rec getVars (ls : List Term) := match ls with
                | [] => []
                | Term.var v :: rs => v :: getVars rs
                | Term.atom s :: rs => s :: getVars rs
                | Term.compound _ [Term.atom s] :: rs => s :: getVars rs
                | _ :: rs => getVars rs
              let params := match args with | Term.list ts => getVars ts | other => getVars [other]
              let body := Constraint.fact (simplifyAST bodyAst |>.getD (Term.atom "true"))
              IO.println s!"[Defined Rule] {headName}"
              traceRuntime opts fun _ => s!"Rule params: {params}, Body: {repr body}"
              solved := true
              runDynamic opts env mEnv ((headName, params, body) :: rEnv) rest cs'.length
          | Term.compound "anyDecl" [Term.compound "statement" [inner]] => do
              -- 声明文（statement）を解決
              let simpleInner := normalizeParsed inner
              traceRuntime opts fun _ => s!"Solving statement: {repr simpleInner}"
              let solveResults := match simpleInner with
                | Term.compound n as => solve opts mEnv rEnv (Constraint.call n as) [] 0
                | _ => solve opts mEnv rEnv (Constraint.fact simpleInner) [] 0
              if !solveResults.isEmpty then do
                IO.println s!"[Solved Statement] {repr simpleInner}"
                traceRuntime opts fun _ => s!"Solution found: {repr solveResults.head!}" -- 最初の解のみ表示
                solved := true
                runDynamic opts env mEnv rEnv rest cs'.length
              else 
                traceRuntime opts fun _ => s!"No solution for statement."
                pure ()
          | _ -> 
              traceRuntime opts fun _ => s!"AST did not match any top-level pattern: {repr ast}"
              pure ()
      
      if !solved then do
        IO.println s!"No valid interpretation found for this block."
        traceRuntime opts fun _ => "All candidates rejected."
        runDynamic opts env mEnv rEnv (skipToNextLine cs') cs'.length

def main : IO Unit :=
  do
  IO.println "========================================"
  IO.println "   Romanesco Dynamic System v0.6.0      "
  IO.println "========================================"
  
  -- トレース設定
  let opts := Lean.Options.empty
    |>.setBool `trace.romanesco.parser true
    |>.setBool `trace.romanesco.engine true
    |>.setBool `trace.romanesco.runtime true

  let filePath : System.FilePath := "samples/syntax.ro"
  -- ファイルが存在するか確認
  if ← filePath.pathExists then
    let content ← IO.FS.readFile filePath
    runDynamic opts bootstrapGrammar [] [] content.toList 0
  else
    IO.println s!"File not found: {filePath}"
  IO.println "========================================