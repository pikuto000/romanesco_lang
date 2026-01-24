import Romanesco.AST
import Lean.Data.Options

open Romanesco.AST

namespace Romanesco.DynamicParser

def GrammarEnv := List (String × SyntaxRule)
def Cache := List ((SyntaxRule × Nat) × (List (Term × List Char)))

/-- 純粋関数内でのトレース用ヘルパー --/
def withTrace {α} (opts : Lean.Options) (pos : Nat) (msg : Unit → String) (res : Unit → α) : α :=
  if opts.getBool `trace.romanesco.parser then
    dbg_trace s!"trace[romanesco.parser] @{pos}: {msg ()}"
    res ()
  else
    res ()

/-- 空白スキップ --/
partial def skipWs (cs : List Char) : List Char :=
  match cs with
  | [] => []
  | ' ' :: xs | '\t' :: xs | '\n' :: xs | '\r' :: xs => skipWs xs
  | '#' :: xs =>
      let rec skipLine : List Char → List Char | [] => [] | '\n' :: ys => ys | _ :: ys => skipLine ys
      skipWs (skipLine xs)
  | c :: xs => c :: xs

/-- アトムの結合 --/
def consolidate (ts : List Term) : Term :=
  let rec isAtoms (ls : List Term) := match ls with | [] => true | Term.atom _ :: rest | Term.number _ :: rest => isAtoms rest | _ => false
  if isAtoms ts then
    let s := ts.foldl (λ acc t => match t with | Term.atom s' => acc ++ s' | Term.number n => acc ++ toString n | _ => acc) ""
    Term.atom s
  else Term.list ts

def limitResults (rs : List (Term × List Char)) (n : Nat) : List (Term × List Char) :=
  let sorted := rs.toArray.qsort (λ a b => match a, b with | (_, c1), (_, c2) => c1.length < c2.length)
  sorted.toList.take n

mutual
  /-- メモ化非決定パースエンジン --/
  partial def pRuleMemo (opts : Lean.Options) (env : GrammarEnv) (rule : SyntaxRule) (cs : List Char) (fuel : Nat) (cache : Cache) : (List (Term × List Char)) × Cache :=
    let pos := cs.length
    let rec findInC (c : Cache) := match c with | [] => none | item :: rs => match item with | ((r, p), res) => if r == rule && p == pos then some res else findInC rs

    match findInC cache with
    | some res =>
        withTrace opts pos (fun _ => s!"Cache hit for {repr rule} (results: {res.length})") fun _ =>
        (res, cache)
    | none =>
      if fuel == 0 then ([], cache) else
      withTrace opts pos (fun _ => s!"Try {repr rule}") fun _ =>
      let resultPair : (List (Term × List Char)) × Cache := match rule with
        | SyntaxRule.terminal s =>
            let sL := s.toList
            if cs.take sL.length == sL then
               withTrace opts pos (fun _ => s!"Matched terminal '{s}'") fun _ =>
               ([(Term.atom s, cs.drop sL.length)], cache)
            else
               withTrace opts pos (fun _ => s!"Failed terminal '{s}'") fun _ =>
               ([], cache)
        | SyntaxRule.anyChar =>
            match cs with
            | c :: xs => ([(Term.atom (String.ofList [c]), xs)], cache)
            | [] => ([], cache)
        | SyntaxRule.charRange c1 c2 =>
            match cs with
            | c :: xs =>
                if c >= c1 && c <= c2 then ([(Term.atom (String.ofList [c]), xs)], cache)
                else
                  withTrace opts pos (fun _ => s!"Failed charRange [{c1}-{c2}], got '{c}'") fun _ =>
                  ([], cache)
            | [] => ([], cache)
        | SyntaxRule.nonTerminal name =>
            let rec findE (e : GrammarEnv) := match e with | [] => none | pair :: rs => match pair with | (n, b) => if n == name then some b else findE rs
            match findE env with
            | some body =>
                let (res, c') := pRuleMemo opts env body cs (fuel - 1) cache
                (res.map (λ pair => (Term.list [Term.atom name, pair.1], pair.2)), c')
            | none =>
                if name == "ANY_STRING" then
                  let s := cs.takeWhile (λ c => c != '"')
                  ([(Term.atom (String.ofList s), cs.drop s.length)], cache)
                else
                  withTrace opts pos (fun _ => s!"Unknown non-terminal '{name}'") fun _ =>
                  ([], cache)
        | SyntaxRule.seq rules =>
            let (seqRes, cFinal) := pSeq opts env rules cs (fuel - 1) cache
            let rec mapL (ls : List (List Term × List Char)) := match ls with | [] => [] | item :: rest => match item with | (ts, fs) => (Term.list ts, fs) :: mapL rest
            (mapL seqRes, cFinal)
        | SyntaxRule.alt rules =>
            let (altRes, cFinal) := pAlt opts env rules cs (fuel - 1) cache
            (limitResults altRes 10, cFinal)
        | SyntaxRule.many r =>
            match pManyL opts env r cs fuel cache with
            | ((ts, fs), cFinal) => ([(consolidate ts, fs)], cFinal)
        | SyntaxRule.many1 r =>
            match pRuleMemo opts env r cs (fuel - 1) cache with
            | (firstRes, c1) =>
                let rec loop (nx : List (Term × List Char)) (currC : Cache) :=
                  match nx with
                  | [] => ([], currC)
                  | (t, nc) :: tail =>
                      match pManyL opts env r nc (fuel - 1) currC with
                      | ((ts, fs), nextC) =>
                          let (others, finalC) := loop tail nextC
                          ((consolidate (t :: ts), fs) :: others, finalC)
                loop (limitResults firstRes 5) c1
        | SyntaxRule.categorized cat r =>
            let cs' := match cat with | LexicalCategory.keyword | LexicalCategory.delimiter | LexicalCategory.separator => skipWs cs | _ => cs
            pRuleMemo opts env r cs' (fuel - 1) cache

      match resultPair with
      | (res, finalC) =>
          withTrace opts pos (fun _ => if res.isEmpty then s!"Failed {repr rule}" else s!"Success {repr rule} -> {res.length} results") fun _ =>
          (res, ((rule, pos), res) :: finalC)

  partial def pSeq (opts : Lean.Options) (env : GrammarEnv) (rs : List SyntaxRule) (cs : List Char) (f : Nat) (c : Cache) : List (List Term × List Char) × Cache :=
    match rs with
    | [] => ([([], cs)], c)
    | r :: rs' =>
        let (nexts, c1) := pRuleMemo opts env r cs f c
        let rec loop (nx : List (Term × List Char)) (currC : Cache) :=
          match nx with
          | [] => ([], currC)
          | (t, nc) :: tail =>
              let (subs, c2) := pSeq opts env rs' nc f currC
              let rec mapA (ls : List (List Term × List Char)) := match ls with | [] => [] | (ts, fs) :: rest => (t :: ts, fs) :: mapA rest
              let (others, c3) := loop tail c2
              (List.append (mapA subs) others, c3)
        loop (limitResults nexts 5) c1

  partial def pAlt (opts : Lean.Options) (env : GrammarEnv) (rs : List SyntaxRule) (cs : List Char) (f : Nat) (c : Cache) : List (Term × List Char) × Cache :=
    match rs with
    | [] => ([], c)
    | r :: rs' =>
        let (res1, c1) := pRuleMemo opts env r cs f c
        let (res2, c2) := pAlt opts env rs' cs f c1
        (List.append res1 res2, c2)

  partial def pManyL (opts : Lean.Options) (env : GrammarEnv) (r : SyntaxRule) (cc : List Char) (f : Nat) (c : Cache) : (List Term × List Char) × Cache :=
    if f == 0 then (([], cc), c) else
    match pRuleMemo opts env r cc f c with
    | (nexts, c1) =>
        let rec findBest (ls : List (Term × List Char)) : Option (Term × List Char) :=
          match ls with
          | [] => none
          | (t, cr) :: rs' => if cr.length < cc.length then
            match findBest rs' with
            | some (bt, bc) => if bc.length < cr.length then some (bt, bc) else some (t, cr)
            | none => some (t, cr) else findBest rs'
        match findBest nexts with
        | some (t, n) =>
            match pManyL opts env r n (f - 1) c1 with
            | (resPairVal, cF) => match resPairVal with | (ts, fs) => ((t :: ts, fs), cF)
        | none => (([], cc), c1)
end

def pRule (opts : Lean.Options) (env : GrammarEnv) (rule : SyntaxRule) (cs : List Char) (fuel : Nat) : List (Term × List Char) := (pRuleMemo opts env rule cs fuel []).fst

/-- 構文木の正規化 --/
partial def normalizeParsed (t : Term) : Term :=
  match t with
  | Term.list [Term.atom name, body] => 
      let body' := normalizeParsed body
      match body' with
      | Term.list ts => Term.compound name ts
      | other => Term.compound name [other]
  | Term.list ts => Term.list (ts.map normalizeParsed)
  | other => other

partial def compileRule (t : Term) : Option SyntaxRule := match t with
  | Term.list [Term.atom "", Term.atom s, Term.atom ""]
 => some (SyntaxRule.terminal s)
  | Term.atom name => if name == "any()" then some SyntaxRule.anyChar else some (SyntaxRule.nonTerminal name)
  | Term.list ts =>
      let rec filterTrash (ls : List Term) := match ls with | [] => [] | Term.atom "" :: rs' => filterTrash rs' | Term.list [] :: rs' => filterTrash rs' | x :: rs' => x :: filterTrash rs'
      let items := filterTrash ts
      match items with
      | [Term.atom "range", Term.atom "(", Term.atom s1, Term.atom ",", Term.atom s2, Term.atom ")"] =>
          match s1.toList.head?, s2.toList.head? with | some c1, some c2 => some (SyntaxRule.charRange c1 c2) | _, _ => none
      | [Term.atom "(", Term.list subs, Term.atom ")"] => let rs := subs.filterMap compileRule; if rs.isEmpty then none else some (SyntaxRule.seq rs)
      | [Term.atom "[", Term.list subs, Term.atom "]"] => let rs := items.filterMap compileRule; if rs.isEmpty then none else some (SyntaxRule.alt rs)
      | [inner, Term.atom "*"] => (compileRule inner).map SyntaxRule.many
      | [inner, Term.atom "+"] => (compileRule inner).map SyntaxRule.many1
      | _ => let rs := items.filterMap compileRule; if rs.isEmpty then none else if rs.length == 1 then some rs[0]! else some (SyntaxRule.seq rs)
  | _ => none

def bootstrapGrammar : GrammarEnv :=
  [ ("anyDecl", SyntaxRule.alt [SyntaxRule.nonTerminal "syntaxDef", SyntaxRule.nonTerminal "ruleDef", SyntaxRule.nonTerminal "statement"]),
    ("WS", SyntaxRule.alt [SyntaxRule.terminal " ", SyntaxRule.terminal "\t", SyntaxRule.terminal "\n", SyntaxRule.terminal "\r"]),
    ("ws", SyntaxRule.many (SyntaxRule.nonTerminal "WS")),
    ("LOWER", SyntaxRule.charRange 'a' 'z'),
    ("UPPER", SyntaxRule.charRange 'A' 'Z'),
    ("DIGIT", SyntaxRule.charRange '0' '9'),
    ("IDENT_CHAR", SyntaxRule.alt [SyntaxRule.nonTerminal "LOWER", SyntaxRule.nonTerminal "UPPER", SyntaxRule.nonTerminal "DIGIT", SyntaxRule.terminal "_"]),
    ("B_IDENT", SyntaxRule.many1 (SyntaxRule.nonTerminal "IDENT_CHAR")),
    ("VAR", SyntaxRule.seq [SyntaxRule.charRange 'A' 'Z', SyntaxRule.many (SyntaxRule.nonTerminal "IDENT_CHAR")]),
    ("syntaxDef", SyntaxRule.seq [
      SyntaxRule.categorized LexicalCategory.keyword (SyntaxRule.terminal "syntax"),
      SyntaxRule.nonTerminal "ws",
      SyntaxRule.nonTerminal "B_IDENT",
      SyntaxRule.nonTerminal "ws",
      SyntaxRule.terminal "=",
      SyntaxRule.nonTerminal "ws",
      SyntaxRule.nonTerminal "meta_rule"
    ]),
    ("ruleDef", SyntaxRule.seq [
      SyntaxRule.categorized LexicalCategory.keyword (SyntaxRule.terminal "rule"),
      SyntaxRule.nonTerminal "ws",
      SyntaxRule.nonTerminal "B_IDENT",
      SyntaxRule.nonTerminal "ws",
      SyntaxRule.terminal "(",
      SyntaxRule.many (SyntaxRule.alt [SyntaxRule.nonTerminal "VAR", SyntaxRule.nonTerminal "B_IDENT", SyntaxRule.terminal "\"", SyntaxRule.terminal ",", SyntaxRule.terminal " ", SyntaxRule.terminal "="]),
      SyntaxRule.terminal ")",
      SyntaxRule.nonTerminal "ws",
      SyntaxRule.terminal ":-",
      SyntaxRule.nonTerminal "ws",
      SyntaxRule.nonTerminal "meta_rule"
    ]),
    ("meta_atom", SyntaxRule.alt [SyntaxRule.nonTerminal "meta_terminal", SyntaxRule.nonTerminal "meta_range", SyntaxRule.nonTerminal "meta_any", SyntaxRule.nonTerminal "meta_seq_paren", SyntaxRule.nonTerminal "meta_alt_paren", SyntaxRule.nonTerminal "meta_nonTerminal"]),
    ("meta_rule", SyntaxRule.alt [SyntaxRule.nonTerminal "meta_many", SyntaxRule.nonTerminal "meta_many1", SyntaxRule.nonTerminal "meta_atom"]),
    ("meta_terminal", SyntaxRule.seq [SyntaxRule.terminal "\"", SyntaxRule.nonTerminal "ANY_STRING", SyntaxRule.terminal "\""]),
    ("meta_range", SyntaxRule.seq [
      SyntaxRule.categorized LexicalCategory.keyword (SyntaxRule.terminal "range"),
      SyntaxRule.nonTerminal "ws",
      SyntaxRule.terminal "(",
      SyntaxRule.nonTerminal "ws",
      SyntaxRule.nonTerminal "meta_terminal",
      SyntaxRule.nonTerminal "ws",
      SyntaxRule.terminal ",",
      SyntaxRule.nonTerminal "ws",
      SyntaxRule.nonTerminal "meta_terminal",
      SyntaxRule.nonTerminal "ws",
      SyntaxRule.terminal ")"
    ]),
    ("meta_any", SyntaxRule.categorized LexicalCategory.keyword (SyntaxRule.terminal "any()")),
    ("meta_nonTerminal", SyntaxRule.nonTerminal "B_IDENT"),
    ("meta_seq_paren", SyntaxRule.seq [
      SyntaxRule.terminal "(",
      SyntaxRule.many (SyntaxRule.seq [SyntaxRule.nonTerminal "ws", SyntaxRule.nonTerminal "meta_rule"]),
      SyntaxRule.nonTerminal "ws",
      SyntaxRule.terminal ")"
    ]),
    ("meta_alt_paren", SyntaxRule.seq [
      SyntaxRule.terminal "[",
      SyntaxRule.many (SyntaxRule.seq [SyntaxRule.nonTerminal "ws", SyntaxRule.nonTerminal "meta_rule"]),
      SyntaxRule.nonTerminal "ws",
      SyntaxRule.terminal "]"
    ]),
    ("meta_many", SyntaxRule.seq [SyntaxRule.nonTerminal "meta_atom", SyntaxRule.nonTerminal "ws", SyntaxRule.terminal "*"]),
    ("meta_many1", SyntaxRule.seq [SyntaxRule.nonTerminal "meta_atom", SyntaxRule.nonTerminal "ws", SyntaxRule.terminal "+"]),
    ("statement", SyntaxRule.terminal "placeholder")
  ]

end Romanesco.DynamicParser
