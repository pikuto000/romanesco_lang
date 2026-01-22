import Romanesco.AST

open Romanesco.AST

namespace Romanesco.DynamicParser

def GrammarEnv := List (String × SyntaxRule)
def Cache := List ((SyntaxRule × Nat) × (List (Term × List Char)))

partial def skipWs (cs : List Char) : List Char :=
  match cs with 
  | [] => [] 
  | ' ' :: xs | '\t' :: xs | '\n' :: xs | '\r' :: xs => skipWs xs 
  | '#' :: xs => 
      let rec skipLine : List Char → List Char | [] => [] | '\n' :: ys => ys | _ :: ys => skipLine ys
      skipWs (skipLine xs)
  | c :: xs => c :: xs

def consolidate (ts : List Term) : Term :=
  let rec isAtoms (ls : List Term) := match ls with | [] => true | Term.atom _ :: rs | Term.number _ :: rs => isAtoms rs | _ => false
  if isAtoms ts then
    let s := ts.foldl (λ acc t => match t with | Term.atom s' => acc ++ s' | Term.number n => acc ++ toString n | _ => acc) ""
    Term.atom s
  else Term.list ts

def limit (rs : List (Term × List Char)) (n : Nat) : List (Term × List Char) :=
  let sorted := rs.toArray.qsort (λ a b => match a, b with | (_, c1), (_, c2) => c1.length < c2.length)
  sorted.toList.take n

/-- メモ化パーサー (絶対安定版) --/
partial def pRuleMemo (env : GrammarEnv) (rule : SyntaxRule) (cs : List Char) (fuel : Nat) (cache : Cache) : (List (Term × List Char)) × Cache :=
  let pos := cs.length
  let rec findInC (c : Cache) := match c with | [] => none | ((r, p), res) :: rest => if r == rule && p == pos then some res else findInC rest
  
  match findInC cache with
  | some res => (res, cache)
  | none =>
    if fuel == 0 then ([], cache) else
    let rec go (r : SyntaxRule) (cc : List Char) (f : Nat) (ccache : Cache) : (List (Term × List Char)) × Cache :=
      match r with
      | SyntaxRule.terminal s => 
          let sL := s.toList
          if cc.take sL.length == sL then ([(Term.atom s, cc.drop sL.length)], ccache) else ([], ccache)
      | SyntaxRule.anyChar => match cc with | c' :: xs => ([(Term.atom (String.ofList [c']), xs)], ccache) | [] => ([], ccache)
      | SyntaxRule.charRange c1 c2 => match cc with | c' :: xs => if c' >= c1 && c' <= c2 then ([(Term.atom (String.ofList [c']), xs)], ccache) else ([], ccache) | [] => ([], ccache)
      | SyntaxRule.nonTerminal name =>
          let rec findEnv (e : GrammarEnv) := match e with | [] => none | (n, b) :: rest => if n == name then some b else findEnv rest
          match findEnv env with
          | some body => pRuleMemo env body cc (f - 1) ccache
          | none => ([], ccache)
      | SyntaxRule.seq rules =>
          let rec pSeq (rs : List SyntaxRule) (ccc : List Char) (ff : Nat) (ccache' : Cache) : List (List Term × List Char) × Cache :=
            match rs with
            | [] => ([([], ccc)], ccache')
            | x :: xs =>
                let (nexts, c') := go x ccc ff ccache'
                let rec loop (nx : List (Term × List Char)) (currC : Cache) :=
                  match nx with
                  | [] => ([], currC)
                  | (t_val, nCs) :: rest =>
                      let (subs, nextC) := pSeq xs nCs ff currC
                      let rec mapA (ls : List (List Term × List Char)) := match ls with | [] => [] | (ts, fs) :: rest' => (t_val :: ts, fs) :: mapA rest'
                      let (others, finalC) := loop rest nextC
                      (List.append (mapA subs) others, finalC)
                loop (limit nexts 5) c'
          let (seqRes, cFinal) := pSeq rules cc (f - 1) ccache
          let rec mapL (ls : List (List Term × List Char)) := match ls with | [] => [] | (ts, fs) :: rest => (Term.list ts, fs) :: mapL rest
          (mapL seqRes, cFinal)
      | SyntaxRule.alt rules => 
          let rec pAlt (rs : List SyntaxRule) (ccache' : Cache) : List (Term × List Char) × Cache :=
            match rs with | [] => ([], ccache') | x :: xs => let (r1, c1) := go x cc (f - 1) ccache'; let (r2, c2) := pAlt xs c1; (List.append r1 r2, c2)
          let (altRes, cFinal) := pAlt rules ccache; (limit altRes 10, cFinal)
      | SyntaxRule.many r' =>
          let rec pManyLoop (cc' : List Char) (ff : Nat) (cnt : Nat) (ccache' : Cache) : (List Term × List Char) × Cache :=
            if ff == 0 || cnt > 20 then (([], cc'), ccache') else
            let (nexts, c1) := go r' cc' ff ccache'
            let rec fB (ls : List (Term × List Char)) := match ls with | [] => none | (t, cr) :: rest => if cr.length < cc'.length then match fB rest with | some (tb, cb) => if cb.length < cr.length then some (tb, cb) else some (t, cr) | none => some (t, cr) else fB rest
            match fB nexts with 
            | some (t, n) => let (resP, cF) := pManyLoop n (ff - 1) (cnt + 1) c1; ((t :: resP.1, resP.2), cF)
            | none => (([], cc'), c1)
          let (mPair, cFinal) := pManyLoop cc (f - 1) 0 ccache; ([(consolidate mPair.1, mPair.2)], cFinal)
      | SyntaxRule.categorized _ r' => go r' cc (f - 1) ccache

    let (resFinal, cacheFinal) := go rule cs fuel cache
    (resFinal, ((rule, pos), resFinal) :: cacheFinal)

def pRule (env : GrammarEnv) (rule : SyntaxRule) (cs : List Char) (fuel : Nat) : List (Term × List Char) := (pRuleMemo env rule cs fuel []).fst

partial def compileRule (t : Term) : Option SyntaxRule := match t with
  | Term.list [Term.atom "\"", Term.atom s, Term.atom "\""] => some (SyntaxRule.terminal s)
  | Term.atom name => if name == "any()" then some SyntaxRule.anyChar else some (SyntaxRule.nonTerminal name)
  | Term.list ts => match ts with
      | [Term.atom "(", Term.list items, Term.atom ")"] => let rs := items.filterMap compileRule; if rs.isEmpty then none else some (SyntaxRule.seq rs)
      | [Term.atom "[", Term.list items, Term.atom "]"] => let rs := items.filterMap compileRule; if rs.isEmpty then none else some (SyntaxRule.alt rs)
      | [inner, Term.atom "*"] => (compileRule inner).map SyntaxRule.many
      | [Term.atom "range", Term.atom "(", Term.atom "\"", Term.atom s1, Term.atom "\"", Term.atom ",", Term.atom "\"", Term.atom s2, Term.atom "\"", Term.atom ")"] => (match s1.toList.head?, s2.toList.head? with | some c1, some c2 => some (SyntaxRule.charRange c1 c2) | _, _ => none)
      | items => let rs := items.filterMap compileRule; if rs.isEmpty then none else if rs.length == 1 then some rs[0]! else some (SyntaxRule.seq rs)
  | _ => none

def bootstrapGrammar : GrammarEnv := [
  ("anyDecl", SyntaxRule.alt [SyntaxRule.nonTerminal "syntaxDef", SyntaxRule.nonTerminal "statement"]),
  ("WS", SyntaxRule.alt [SyntaxRule.terminal " ", SyntaxRule.terminal "\t", SyntaxRule.terminal "\n", SyntaxRule.terminal "\r"]),
  ("ws", SyntaxRule.many (SyntaxRule.nonTerminal "WS")),
  ("LOWER", SyntaxRule.charRange 'a' 'z'),
  ("UPPER", SyntaxRule.charRange 'A' 'Z'),
  ("DIGIT", SyntaxRule.charRange '0' '9'),
  ("IDENT_CHAR", SyntaxRule.alt [SyntaxRule.nonTerminal "LOWER", SyntaxRule.nonTerminal "UPPER", SyntaxRule.nonTerminal "DIGIT", SyntaxRule.terminal "_"]),
  ("B_IDENT", SyntaxRule.many (SyntaxRule.nonTerminal "IDENT_CHAR")),
  ("syntaxDef", SyntaxRule.seq [SyntaxRule.terminal "syntax", SyntaxRule.nonTerminal "ws", SyntaxRule.nonTerminal "B_IDENT", SyntaxRule.nonTerminal "ws", SyntaxRule.terminal "=", SyntaxRule.nonTerminal "ws", SyntaxRule.nonTerminal "meta_rule"]),
  ("meta_atom", SyntaxRule.alt [SyntaxRule.nonTerminal "meta_terminal", SyntaxRule.nonTerminal "meta_range", SyntaxRule.nonTerminal "meta_any", SyntaxRule.nonTerminal "meta_seq_paren", SyntaxRule.nonTerminal "meta_alt_paren", SyntaxRule.nonTerminal "meta_nonTerminal"]),
  ("meta_rule", SyntaxRule.alt [SyntaxRule.nonTerminal "meta_many", SyntaxRule.nonTerminal "meta_atom"]),
  ("meta_terminal", SyntaxRule.seq [SyntaxRule.terminal "\"", SyntaxRule.nonTerminal "ANY_STRING", SyntaxRule.terminal "\""]),
  ("meta_range", SyntaxRule.seq [SyntaxRule.terminal "range", SyntaxRule.nonTerminal "ws", SyntaxRule.terminal "(", SyntaxRule.nonTerminal "ws", SyntaxRule.terminal "\"", SyntaxRule.nonTerminal "ANY_STRING", SyntaxRule.terminal "\"", SyntaxRule.nonTerminal "ws", SyntaxRule.terminal ",", SyntaxRule.nonTerminal "ws", SyntaxRule.terminal "\"", SyntaxRule.nonTerminal "ANY_STRING", SyntaxRule.terminal "\"", SyntaxRule.nonTerminal "ws", SyntaxRule.terminal ")"]),
  ("meta_any", SyntaxRule.terminal "any()") ,
  ("meta_nonTerminal", SyntaxRule.nonTerminal "B_IDENT"),
  ("meta_seq_paren", SyntaxRule.seq [SyntaxRule.terminal "(", SyntaxRule.many (SyntaxRule.seq [SyntaxRule.nonTerminal "ws", SyntaxRule.nonTerminal "meta_rule"] ), SyntaxRule.nonTerminal "ws", SyntaxRule.terminal ")"]),
  ("meta_alt_paren", SyntaxRule.seq [SyntaxRule.terminal "[", SyntaxRule.many (SyntaxRule.seq [SyntaxRule.nonTerminal "ws", SyntaxRule.nonTerminal "meta_rule"] ), SyntaxRule.nonTerminal "ws", SyntaxRule.terminal "]"]),
  ("meta_many", SyntaxRule.seq [SyntaxRule.nonTerminal "meta_atom", SyntaxRule.nonTerminal "ws", SyntaxRule.terminal "*"]),
  ("statement", SyntaxRule.terminal "placeholder")
]

end Romanesco.DynamicParser