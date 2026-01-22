import Romanesco.AST

open Romanesco.AST

namespace Romanesco.Parser

def Result (α : Type) := List (α × List Char)

inductive Target | Term | Args | Const | Full | Decl

structure POut where
  term : Option Term := none
  args : Option (List Term) := none
  constraint : Option Constraint := none
  decl : Option Declaration := none
  rest : List Char
  deriving Inhabited

def isIdChar (c : Char) : Bool := c.isAlphanum || c == '_'

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

def removeWs (s : String) : String :=
  let cs := s.toList
  let rec trimLeft : List Char → List Char
    | [] => []
    | c :: cs => if c == ' ' || c == '\t' || c == '\n' || c == '\r' then trimLeft cs else c :: cs
  let rec trimRight (cs : List Char) : List Char :=
    trimLeft cs.reverse |>.reverse
  String.ofList (trimRight (trimLeft cs))

def takeWhile (p : Char → Bool) (cs : List Char) : List Char × List Char :=
  let res := cs.takeWhile p
  (res, cs.drop res.length)

partial def pAll (t : Target) (cs : List Char) : List POut :=
  let cs' := skipWs cs
  match t with
  | Target.Term => 
      match cs' with
      | [] => []
      | '?' :: rest => 
          let p := takeWhile isIdChar rest
          if p.1.isEmpty then [] else [{ rest := skipWs p.2, term := some (Term.var (String.ofList p.1)) }]
      | '[' :: rest =>
          let rest := skipWs rest
          match rest with
          | ']' :: r => [{ rest := r, term := some (Term.list []) }]
          | _ =>
              (pAll Target.Args rest).filterMap (fun res =>
                match res.args with
                | some ts =>
                    match skipWs res.rest with
                    | ']' :: r => some { rest := r, term := some (Term.list ts) }
                    | _ => none
                | _ => none)
      | x :: xs =>
          if x.isDigit then
            let p := takeWhile (fun c => c.isDigit) (x :: xs)
            [{ rest := skipWs p.2, term := some (Term.number (String.ofList p.1 |>.toNat? |>.getD 0)) }]
          else if isIdChar x then
            let p := takeWhile isIdChar (x :: xs)
            let name := String.ofList p.1
            let r' := skipWs p.2
            match r' with
            | '(' :: argsStart => 
                let argsStart' := skipWs argsStart
                match argsStart' with
                | ')' :: r'' => [{ rest := skipWs r'', term := some (Term.compound name []) }]
                | _ =>
                    (pAll Target.Args argsStart').filterMap (fun res =>
                      match res.args with
                      | some ts => 
                          match skipWs res.rest with
                          | ')' :: r'' => some { rest := skipWs r'', term := some (Term.compound name ts) }
                          | _ => none
                      | _ => none)
            | _ => [{ rest := skipWs p.2, term := some (Term.atom name) }]
          else []
  | Target.Args => 
      let resTerms := pAll Target.Term cs'
      List.flatten (resTerms.map (fun res =>
        match res.term with
        | some termVal =>
            let r' := skipWs res.rest
            match r' with
            | ',' :: rest => (pAll Target.Args rest).filterMap (fun r2 =>
                match r2.args with
                | some ts => some { rest := r2.rest, args := some (termVal :: ts) }
                | _ => none)
            | _ => [{ rest := res.rest, args := some [termVal] }]
        | _ => []
      ))
  | Target.Const =>
      let resTerms := pAll Target.Term cs'
      List.flatten (resTerms.map (fun res =>
        match res.term with
        | some t1 =>
            let r1 := skipWs res.rest
            match r1 with
            | '=' :: r2 => (pAll Target.Term r2).filterMap (fun r2 =>
                match r2.term with
                | some t2 => some { rest := r2.rest, constraint := some (Constraint.equal t1 t2) }
                | _ => none)
            | _ => 
                match t1 with
                | Term.compound name args => [{ rest := res.rest, constraint := some (Constraint.call name args) }]
                | _ => [{ rest := res.rest, constraint := some (Constraint.fact t1) }]
        | _ => []
      ))
  | Target.Full =>
      let resConsts := pAll Target.Const cs'
      List.flatten (resConsts.map (fun res =>
        match res.constraint with
        | some c1 =>
            let r1 := skipWs res.rest
            match r1 with
            | '&' :: r2 => (pAll Target.Full r2).filterMap (fun r2 =>
                match r2.constraint with
                | some c2 => some { rest := r2.rest, constraint := some (Constraint.conj c1 c2) }
                | _ => none)
            | _ => [{ rest := res.rest, constraint := some c1 }]
        | _ => []
      ))
  | Target.Decl =>
      let pref := cs'.take 5
      if String.ofList pref == "macro" then
        let p := takeWhile isIdChar (skipWs (cs'.drop 5))
        if p.1.isEmpty then [] else
        let name := String.ofList p.1
        match skipWs p.2 with
        | '(' :: r2 =>
            let p2 := takeWhile (fun c => c != ')') r2
            let params := (String.ofList p2.1).splitOn "," |>.map removeWs |>.filter (fun s => s != "")
            match skipWs p2.2 with
            | ')' :: r4 =>
                match skipWs r4 with
                | '=' :: r5 => 
                    (pAll Target.Term r5).filterMap (fun res =>
                      match res.term with
                      | some body => some { rest := skipWs res.rest, decl := some (Declaration.macroDef name params body) }
                      | _ => none)
                | _ => []
            | _ => []
        | _ => []
      else if cs'.take 3 == "def".toList then
        let p := takeWhile isIdChar (skipWs (cs'.drop 3))
        if p.1.isEmpty then [] else
        let name := String.ofList p.1
        match skipWs p.2 with
        | '(' :: r2 =>
            let p2 := takeWhile (fun c => c != ')') r2
            let params := (String.ofList p2.1).splitOn "," |>.map removeWs |>.filter (fun s => s != "")
            match skipWs p2.2 with
            | ')' :: r4 =>
                let r5 := skipWs r4
                if r5.take 2 == ":-".toList then
                   (pAll Target.Full (r5.drop 2)).filterMap (fun res =>
                      match res.constraint with
                      | some body => some { rest := skipWs res.rest, decl := some (Declaration.ruleDef (Rule.rule name params body)) }
                      | _ => none)
                else []
            | _ => []
        | _ => []
      else
        let res := pAll Target.Full cs'
        res.filterMap (fun r => 
          match r.constraint with
          | some c => some { rest := r.rest, decl := some (Declaration.action c) }
          | none => none)

partial def programDebug (cs : List Char) : List Declaration × String :=
  let cs' := skipWs cs
  if cs'.isEmpty then ([], "")
  else
    match pAll Target.Decl cs' with
    | res :: _ => 
        match res.decl with
        | some d => 
            let (ds, last) := programDebug res.rest
            (d :: ds, last)
        | none => ([], String.ofList (cs'.take 20))
    | [] => ([], String.ofList (cs'.take 20))

partial def programP (cs : List Char) : List Declaration :=
  (programDebug cs).1

end Romanesco.Parser