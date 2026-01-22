import Romanesco.AST

open Romanesco.AST

namespace Romanesco.DynamicParser

def Result := List (Term × List Char)
def GrammarEnv := List (String × SyntaxRule)

partial def skipLine (cs : List Char) : List Char :=
  match cs with
  | [] => []
  | '\n' :: xs => xs
  | _ :: xs => skipLine xs

partial def skipWs (cs : List Char) : List Char :=
  match cs with
  | [] => []
  | ' ' :: xs => skipWs xs
  | '\t' :: xs => skipWs xs
  | '\n' :: xs => skipWs xs
  | '\r' :: xs => skipWs xs
  | '#' :: xs => skipWs (skipLine xs)
  | c :: xs => c :: xs

/-- 自動スキップを行わない純粋なパーサー --/
partial def pRule (env : GrammarEnv) (rule : SyntaxRule) (cs : List Char) : Result :=
  match rule with
  | SyntaxRule.terminal s =>
      let sList := s.toList
      if cs.take sList.length == sList then
        [(Term.atom s, cs.drop sList.length)]
      else []
  
  | SyntaxRule.nonTerminal name =>
      match env.find? (fun p => p.1 == name) with
      | some (_, body) => pRule env body cs
      | none => 
          if name == "IDENTIFIER" then
            let id := cs.takeWhile (fun c => c.isAlphanum || c == '_')
            if id.isEmpty then [] else [(Term.atom (String.ofList id), cs.drop id.length)]
          else if name == "NUMBER" then
            let ds := cs.takeWhile (fun c => c.isDigit)
            if ds.isEmpty then [] else [(Term.number ((String.ofList ds).toNat? |>.getD 0), cs.drop ds.length)]
          else if name == "ANY_STRING" then
            let s := cs.takeWhile (fun c => c != '"')
            [(Term.atom (String.ofList s), cs.drop s.length)]
          else if name == "SPACE" then
            match cs with | ' ' :: xs => [(Term.atom " ", xs)] | _ => []
          else if name == "TAB" then
            match cs with | '\t' :: xs => [(Term.atom "\t", xs)] | _ => []
          else if name == "NEWLINE" then
            match cs with
            | '\n' :: xs => [(Term.atom "\n", xs)]
            | '\r' :: '\n' :: xs => [(Term.atom "\r\n", xs)]
            | '\r' :: xs => [(Term.atom "\r", xs)]
            | _ => []
          else if name == "WS" then
            match cs with
            | c :: xs => if c == ' ' || c == '\t' || c == '\n' || c == '\r' then [(Term.atom (String.ofList [c]), xs)] else []
            | _ => []
          else if name == "ANY_CHAR_NOT_NEWLINE" then
            match cs with
            | c :: xs => if c != '\n' && c != '\r' then [(Term.atom (String.ofList [c]), xs)] else []
            | _ => []
          else []
  
  | SyntaxRule.seq rules =>
      let rec pSeq : List SyntaxRule → List Char → List (List Term × List Char)
        | [], currCs => [([], currCs)]
        | r :: rest, currCs =>
            List.flatten ((pRule env r currCs).map (fun (t, nextCs) =>
              (pSeq rest nextCs).map (fun (ts, finalCs) => (t :: ts, finalCs))))
      (pSeq rules cs).map (fun (ts, r) => (Term.list ts, r))

  | SyntaxRule.alt rules =>
      List.flatten (rules.map (fun r => pRule env r cs))

  | SyntaxRule.many r =>
      let rec pMany (currCs : List Char) : List (List Term × List Char) :=
        let nexts := pRule env r currCs
        let filteredNexts := nexts.filter (fun p => p.2.length < currCs.length)
        if filteredNexts.isEmpty then [([], currCs)]
        else
          List.append
            (List.flatten (filteredNexts.map (fun (t, nextCs) =>
              (pMany nextCs).map (fun (ts, finalCs) => (t :: ts, finalCs)))))
            [([], currCs)]
      (pMany cs).map (fun (ts, nextCs) => (Term.list ts, nextCs))

/-- ブートストラップ文法 (空白を明示的に扱う) --/
def bootstrapGrammar : GrammarEnv := [
  ("decl", SyntaxRule.alt [
    SyntaxRule.nonTerminal "syntaxDef",
    SyntaxRule.nonTerminal "ws",
    SyntaxRule.nonTerminal "comment",
    SyntaxRule.nonTerminal "anyConstraint"
  ]),
  ("ws", SyntaxRule.many (SyntaxRule.nonTerminal "WS")),
  ("comment", SyntaxRule.seq [
    SyntaxRule.terminal "#",
    SyntaxRule.many (SyntaxRule.nonTerminal "ANY_CHAR_NOT_NEWLINE"),
    SyntaxRule.nonTerminal "NEWLINE"
  ]),
  ("syntaxDef", SyntaxRule.seq [
    SyntaxRule.terminal "syntax",
    SyntaxRule.nonTerminal "ws",
    SyntaxRule.nonTerminal "IDENTIFIER",
    SyntaxRule.nonTerminal "ws",
    SyntaxRule.terminal "=",
    SyntaxRule.nonTerminal "ws",
    SyntaxRule.nonTerminal "rule"
  ]),
  ("rule", SyntaxRule.alt [
    SyntaxRule.nonTerminal "terminalRule",
    SyntaxRule.nonTerminal "seqRule",
    SyntaxRule.nonTerminal "nonTerminalRule"
  ]),
  ("terminalRule", SyntaxRule.seq [
    SyntaxRule.terminal "\"",
    SyntaxRule.nonTerminal "ANY_STRING",
    SyntaxRule.terminal "\""
  ]),
  ("nonTerminalRule", SyntaxRule.nonTerminal "IDENTIFIER"),
  ("seqRule", SyntaxRule.seq [
    SyntaxRule.terminal "(",
    SyntaxRule.many (SyntaxRule.seq [SyntaxRule.nonTerminal "ws", SyntaxRule.nonTerminal "rule"]),
    SyntaxRule.nonTerminal "ws",
    SyntaxRule.terminal ")"
  ]),
  ("anyConstraint", SyntaxRule.seq [
    SyntaxRule.nonTerminal "IDENTIFIER",
    SyntaxRule.nonTerminal "ws",
    SyntaxRule.terminal "=",
    SyntaxRule.nonTerminal "ws",
    SyntaxRule.nonTerminal "NUMBER"
  ])
]

partial def compileRule (t : Term) : Option SyntaxRule :=
  match t with
  | Term.list [Term.atom "\"", Term.atom s, Term.atom "\""] => some (SyntaxRule.terminal s)
  | Term.atom name => some (SyntaxRule.nonTerminal name)
  | _ => none

end Romanesco.DynamicParser