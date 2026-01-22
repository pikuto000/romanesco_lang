import Romanesco.AST

open Romanesco.AST

namespace Romanesco.Engine

def MacroEnv := List (String × List String × Term)
def RuleEnv := List (String × List String × Constraint)
def Substitution := List (String × Term)

/-- マクロ展開 -/
partial def expand (env : MacroEnv) (t : Term) : Term :=
  match t with
  | Term.compound name args =>
    let expandedArgs := args.map (expand env)
    match env.find? (fun p => p.1 == name) with
    | some (_, params, body) =>
      let rec substTerm : Term → Term
        | Term.var v =>
            match (params.zip expandedArgs).find? (fun p => p.1 == v) with
            | some p => p.2
            | none => Term.var v
        | Term.compound n as => Term.compound n (as.map substTerm)
        | Term.list ts => Term.list (ts.map substTerm)
        | other => other
      expand env (substTerm body)
    | none => Term.compound name expandedArgs
  | Term.list ts => Term.list (ts.map (expand env))
  | other => other

/-- 組み込み計算 -/
def evalBuiltin (name : String) (args : List Term) : Option Term :=
  match name, args with
  | "add", [Term.number n1, Term.number n2] => some (Term.number (n1 + n2))
  | "mul", [Term.number n1, Term.number n2] => some (Term.number (n1 * n2))
  | "sub", [Term.number n1, Term.number n2] => some (Term.number (n1 - n2))
  | "cons", [h, Term.list t] => some (Term.list (h :: t))
  | "first", [Term.list (h :: t)] => some h
  | "rest", [Term.list (h :: t)] => some (Term.list t)
  | _, _ => none

/-- 正規化 -/
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
  | Term.list ts => Term.list (ts.map (normalize subst))
  | other => other

/-- 単一化 -/
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
  | Term.list ts1, Term.list ts2 =>
      if ts1.length == ts2.length then
        (ts1.zip ts2).foldlM (fun s p => unify p.1 p.2 s) subst
      else none
  | _, _ => none

/-- 制約解消 -/
partial def solve (mEnv : MacroEnv) (rEnv : RuleEnv) (c : Constraint) (subst : Substitution) (depth : Nat) : List Substitution :=
  if depth > 100 then [] else
  match c with
  | Constraint.equal t1 t2 =>
      match unify (expand mEnv t1) (expand mEnv t2) subst with
      | some s => [s]
      | none => []
  | Constraint.conj c1 c2 =>
      let ss1 := solve mEnv rEnv c1 subst depth
      ss1.flatMap (fun s1 => solve mEnv rEnv c2 s1 depth)
  | Constraint.disj c1 c2 =>
      solve mEnv rEnv c1 subst depth ++ solve mEnv rEnv c2 subst depth
  | Constraint.call name args =>
      let t := expand mEnv (Term.compound name args)
      match t with
      | Term.compound n as =>
          if n == name && as == args then
            let rules := rEnv.filter (fun r => r.1 == name)
            if rules.isEmpty then [subst]
            else rules.flatMap (fun (_, params, body) =>
              if params.length != args.length then []
              else
                let suffix := s!"_{depth}"
                let body' := body -- 単純化のためリネームなし
                let params' := params.map (fun p => Term.var (p ++ suffix))
                let argEqs := (params'.zip args).foldl (fun acc p => Constraint.conj acc (Constraint.equal p.1 p.2)) (Constraint.fact (Term.atom "true"))
                solve mEnv rEnv (Constraint.conj argEqs body') subst (depth + 1)
            )
          else [subst]
      | _ => [subst]
  | Constraint.fact _ => [subst]

end Romanesco.Engine
