namespace Romanesco.Core

/-- AST --/
inductive Term
  | atom : String → Term
  | number : Nat → Term
  | var : String → Term
  | compound : String → List Term → Term
  deriving Repr, BEq, Inhabited

inductive Constraint
  | equal : Term → Term → Constraint
  | conj : Constraint → Constraint → Constraint
  | fact : Term → Constraint
  deriving Repr, BEq, Inhabited

inductive Declaration
  | macroDef : String → List String → Term → Declaration
  | constraint : Constraint → Declaration
  deriving Repr, Inhabited

/-- 非決定的パーサー基盤 --/
def Result (α : Type) := List (α × List Char)

def pPure (x : α) : List Char → Result α := fun cs => [(x, cs)]

def pBind (p : List Char → Result α) (f : α → List Char → Result β) : List Char → Result β := fun cs =>
  List.flatten ((p cs).map (fun p' => f p'.1 p'.2))

def pOrElse (p1 : List Char → Result α) (p2 : List Char → Result α) : List Char → Result α := fun cs =>
  List.append (p1 cs) (p2 cs)

def pAnyChar : List Char → Result Char
  | [] => []
  | c :: cs => [(c, cs)]

def pChar (c : Char) : List Char → Result Char :=
  pBind pAnyChar (fun c' => if c == c' then pPure c' else fun _ => [])

partial def pSkipWs (cs : List Char) : Result Unit :=
  let skipOne := pOrElse
    (pBind pAnyChar (fun c => if c == ' ' || c == '\t' || c == '\n' || c == '\r' then pPure () else fun _ => []))
    (pBind (pChar '#') (fun _ => 
      let rec skipLine : List Char → List Char
        | [] => []
        | '\n' :: xs => xs
        | _ :: xs => skipLine xs
      fun cs' => [( (), skipLine cs' )]
    ))
  pOrElse (pBind skipOne (fun _ => pSkipWs)) (pPure ()) cs

def pTakeWhile (p : Char → Bool) : List Char → Result (List Char) := fun cs =>
  let res := cs.takeWhile p
  [(res, cs.drop res.length)]

def isIdChar (c : Char) : Bool := c.isAlphanum || c == '_'

def pIdentifier : List Char → Result String :=
  pBind (pTakeWhile isIdChar) (fun cs => 
    if cs.isEmpty then fun _ => [] else pPure (String.ofList cs))

def pNat : List Char → Result Nat :=
  pBind (pTakeWhile (fun c => c >= '0' && c <= '9')) (fun cs =>
    match (String.ofList cs).toNat? with
    | some n => pPure n
    | none => fun _ => [])

/-- Romanesco パーサー (相互再帰) --/
mutual
  partial def pTerm (cs : List Char) : Result Term :=
    pBind pSkipWs (fun _ =>
      pOrElse
        (pBind (pChar '?') (fun _ => pBind pIdentifier (fun id => pPure (Term.var id))))
        (pOrElse
          (pBind pNat (fun n => pPure (Term.number n)))
          (pBind pIdentifier (fun id => 
            pOrElse
              (pBind (pChar '(') (fun _ =>
               pBind pArgs (fun args =>
               pBind (pChar ')') (fun _ =>
               pPure (Term.compound id args))))))
              (pPure (Term.atom id)))))) cs

  partial def pArgs (cs : List Char) : Result (List Term) :=
    pBind pTerm (fun t =>
      pOrElse
        (pBind (pChar ',') (fun _ => pBind pArgs (fun ts => pPure (t :: ts))))
        (pPure [t])) cs
end

partial def pConstraint (cs : List Char) : Result Constraint :=
  pBind pTerm (fun t1 =>
    pBind pSkipWs (fun _ =>
      pOrElse
        (pBind (pChar '=') (fun _ => pBind pTerm (fun t2 => pPure (Constraint.equal t1 t2))))
        (pPure (Constraint.fact t1)))) cs

partial def pFullConstraint (cs : List Char) : Result Constraint :=
  pBind pConstraint (fun c1 =>
    pOrElse
      (pBind (pChar '&') (fun _ => pBind pFullConstraint (fun c2 => pPure (Constraint.conj c1 c2))))
      (pPure c1)) cs

def pMacroDef (cs : List Char) : Result Declaration :=
  pBind (fun cs' => if cs'.take 5 == "macro".toList then [((), cs'.drop 5)] else []) (fun _ =>
  pBind pIdentifier (fun name =>
  pBind (pChar '(') (fun _ =>
  pBind (pOrElse (pBind pIdentifier (fun id => 
    pBind (pTakeWhile (fun c => isIdChar c || c == ',')) (fun rest =>
      let params := (String.ofList (id.toList ++ rest)).splitOn "," |>.map (fun s => s.trim) |>.filter (fun s => s != "")
      pPure params))) (pPure [])) (fun params =>
  pBind (pChar ')') (fun _ =>
  pBind pSkipWs (fun _ =>
  pBind (pChar '=') (fun _ =>
  pBind pTerm (fun body =>
  pPure (Declaration.macroDef name params body)))))))))) cs

def pDeclaration (cs : List Char) : Result Declaration :=
  pOrElse pMacroDef (pBind pFullConstraint (fun c => pPure (Declaration.constraint c))) cs

partial def pProgram (cs : List Char) : List Declaration :=
  match pBind pDeclaration (fun d => pBind pSkipWs (fun _ => pPure d)) cs with
  | (d, rest) :: _ => d :: pProgram rest
  | [] => []

/-- エンジン --/
def MacroEnv := List (String × List String × Term)
def Substitution := List (String × Term)

partial def expand (env : MacroEnv) : Term → Term
  | Term.compound name args =>
    let expandedArgs := args.map (expand env)
    match env.find? (fun p => p.1 == name) with
    | some (_, params, body) =>
      let rec subst : Term → Term
        | Term.var v => 
            match (params.zip expandedArgs).find? (fun p => p.1 == v) with
            | some p => p.2
            | none => Term.var v
        | Term.compound n as => Term.compound n (as.map subst)
        | t => t
      expand env (subst body)
    | none => Term.compound name expandedArgs
  | t => t

def evalBuiltin (name : String) (args : List Term) : Option Term :=
  match name, args with
  | "add", [Term.number n1, Term.number n2] => some (Term.number (n1 + n2))
  | "mul", [Term.number n1, Term.number n2] => some (Term.number (n1 * n2))
  | _, _ => none

partial def normalize (subst : Substitution) (t : Term) : Term :=
  match t with
  | Term.var v => 
      match subst.find? (fun p => p.1 == v) with
      | some (_, t') => normalize subst t'
      | none => Term.var v
  | Term.compound name args =>
      let ns := args.map (normalize subst)
      match evalBuiltin name ns with
      | some res => res
      | none => Term.compound name ns
  | t => t

partial def unify (t1 t2 : Term) (subst : Substitution) : Option Substitution :=
  let n1 := normalize subst t1
  let n2 := normalize subst t2
  if n1 == n2 then some subst
  else match n1, n2 with
  | Term.var v, t | t, Term.var v => some ((v, t) :: subst)
  | Term.compound n1 as1, Term.compound n2 as2 =>
      if n1 == n2 && as1.length == as2.length then
        (as1.zip as2).foldlM (fun s p => unify p.1 p.2 s) subst
      else none
  | _, _ => none

partial def solve (env : MacroEnv) (c : Constraint) (subst : Substitution) : Option Substitution :=
  match c with
  | Constraint.equal t1 t2 => unify (expand env t1) (expand env t2) subst
  | Constraint.conj c1 c2 =>
      match solve env c1 subst with
      | some s => solve env c2 s
      | none => none
  | Constraint.fact _ => some subst

end Romanesco.Core
