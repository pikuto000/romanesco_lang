package romanesco
import scala.collection.mutable
import scala.io.StdIn

// ==========================================
// 1. AST
// ==========================================

enum Ind:
  case Var(name: String)
  case Const(name: String)
  case Meta(id: Int)
  override def toString: String = this match
    case Var(n) => n
    case Const(n) => n
    case Meta(id) => s"?$id"

enum Prop:
  case Pred(name: String, args: List[Ind])
  case Eq(left: Ind, right: Ind)
  case Arrow(from: Prop, to: Prop)
  case And(left: Prop, right: Prop)
  case Or(left: Prop, right: Prop)
  case Forall(v: String, body: Prop)
  case Exists(v: String, body: Prop)
  case True
  case False
  override def toString: String = this match
    case Pred(n, args) => if args.isEmpty then n else s"$n(${args.mkString(",")})"
    case Eq(l, r) => s"$l=$r"
    case Arrow(f, t) => s"($f->$t)"
    case And(l, r) => s"($l&$r)"
    case Or(l, r) => s"($l|$r)"
    case Forall(v, b) => s"forall $v. $b"
    case Exists(v, b) => s"exists $v. $b"
    case True => "T"
    case False => "F"

enum Term:
  case Var(name: String)
  case Abs(param: String, body: Term)
  case App(func: Term, arg: Term)
  case Pair(fst: Term, snd: Term)
  case Fst(term: Term)
  case Snd(term: Term)
  case Inl(term: Term)
  case Inr(term: Term)
  case Case(selector: Term, ln: String, lb: Term, rn: String, rb: Term)
  case UnitTerm
  case Absurd(term: Term)
  case Witness(ind: Ind, proof: Term)
  case Unpack(ex: Term, vName: String, pName: String, body: Term)
  case Refl(ind: Ind)
  case Rewrite(eqProof: Term, targetProof: Term)
  case Inst(func: Term, arg: Ind)

  override def toString: String = this match
    case Var(n) => n
    case Abs(p, b) => s"(\\$p. $b)"
    case App(f, a) => s"($f $a)"
    case Pair(a, b) => s"($a,$b)"
    case Fst(t) => s"fst($t)"
    case Snd(t) => s"snd($t)"
    case Inl(t) => s"inl($t)"
    case Inr(t) => s"inr($t)"
    case Case(s, ln, lb, rn, rb) => s"case $s of {L($ln)=>$lb; R($rn)=>$rb}"
    case UnitTerm => "()"
    case Absurd(t) => s"absurd($t)"
    case Witness(i, p) => s"($i,$p)"
    case Unpack(e, v, p, b) => s"unpack $e as ($v,$p) in $b"
    case Refl(i) => s"refl($i)"
    case Rewrite(eq, p) => s"rewrite($eq,$p)"
    case Inst(f, a) => s"($f [$a])"

// ==========================================
// 2. Unifier
// ==========================================

object Unifier:
  type Subst = Map[Int, Ind]
  def emptySubst: Subst = Map.empty

  def applySubst(t: Ind, s: Subst): Ind = t match
    case Ind.Meta(id) if s.contains(id) => applySubst(s(id), s)
    case _ => t

  def applySubst(p: Prop, s: Subst): Prop = p match
    case Prop.Pred(n, args) => Prop.Pred(n, args.map(applySubst(_, s)))
    case Prop.Eq(l, r) => Prop.Eq(applySubst(l, s), applySubst(r, s))
    case Prop.Arrow(f, t) => Prop.Arrow(applySubst(f, s), applySubst(t, s))
    case Prop.And(l, r) => Prop.And(applySubst(l, s), applySubst(r, s))
    case Prop.Or(l, r) => Prop.Or(applySubst(l, s), applySubst(r, s))
    case Prop.Forall(v, b) => Prop.Forall(v, applySubst(b, s))
    case Prop.Exists(v, b) => Prop.Exists(v, applySubst(b, s))
    case _ => p

  def applySubst(proof: Term, s: Subst): Term = proof match
    case Term.Witness(i, p) => Term.Witness(applySubst(i, s), applySubst(p, s))
    case Term.Refl(i) => Term.Refl(applySubst(i, s))
    case Term.Inst(f, a) => Term.Inst(applySubst(f, s), applySubst(a, s))
    case Term.Abs(v, b) => Term.Abs(v, applySubst(b, s))
    case Term.App(f, a) => Term.App(applySubst(f, s), applySubst(a, s))
    case Term.Pair(a, b) => Term.Pair(applySubst(a, s), applySubst(b, s))
    case Term.Fst(t) => Term.Fst(applySubst(t, s))
    case Term.Snd(t) => Term.Snd(applySubst(t, s))
    case Term.Inl(t) => Term.Inl(applySubst(t, s))
    case Term.Inr(t) => Term.Inr(applySubst(t, s))
    case Term.Case(sel, lv, lb, rv, rb) => Term.Case(applySubst(sel, s), lv, applySubst(lb, s), rv, applySubst(rb, s))
    case Term.Unpack(ex, v, p, b) => Term.Unpack(applySubst(ex, s), v, p, applySubst(b, s))
    case Term.Rewrite(eq, t) => Term.Rewrite(applySubst(eq, s), applySubst(t, s))
    case Term.Absurd(t) => Term.Absurd(applySubst(t, s))
    case _ => proof

  def unifyInd(t1: Ind, t2: Ind, subst: Subst): Option[Subst] =
    val r1 = applySubst(t1, subst)
    val r2 = applySubst(t2, subst)
    if r1 == r2 then Some(subst)
    else (r1, r2) match
      case (Ind.Meta(id), t) => Some(subst + (id -> t))
      case (t, Ind.Meta(id)) => Some(subst + (id -> t))
      case _ => None

  def unifyProp(p1: Prop, p2: Prop, subst: Subst): Option[Subst] =
    (p1, p2) match
      case (Prop.Pred(n1, a1), Prop.Pred(n2, a2)) if n1 == n2 && a1.length == a2.length =>
        a1.zip(a2).foldLeft(Option(subst)) { case (sOpt, (i1, i2)) => sOpt.flatMap(s => unifyInd(i1, i2, s)) }
      case (Prop.Eq(l1, r1), Prop.Eq(l2, r2)) =>
        unifyInd(l1, l2, subst).flatMap(s => unifyInd(r1, r2, s))
      case (Prop.Arrow(f1, t1), Prop.Arrow(f2, t2)) =>
        unifyProp(f1, f2, subst).flatMap(s => unifyProp(t1, t2, s))
      case (Prop.And(l1, r1), Prop.And(l2, r2)) =>
        unifyProp(l1, l2, subst).flatMap(s => unifyProp(r1, r2, s))
      case (Prop.Or(l1, r1), Prop.Or(l2, r2)) =>
        unifyProp(l1, l2, subst).flatMap(s => unifyProp(r1, r2, s))
      case (Prop.True, Prop.True) => Some(subst)
      case (Prop.False, Prop.False) => Some(subst)
      case _ => None

// ==========================================
// 3. Prover
// ==========================================

object Prover:
  import Unifier._

  type Context = List[(String, Prop)]
  var metaCounter = 0

  def freshMeta(): Ind = { metaCounter += 1; Ind.Meta(metaCounter) }

  def prove(target: Prop): Option[Term] =
    metaCounter = 0
    solve(target, Nil, emptySubst, 0).map { case (term, subst) => applySubst(term, subst) }

  private def solve(target: Prop, ctx: Context, subst: Subst, depth: Int): Option[(Term, Subst)] =
    if depth > 25 then None 
    else
      val currentTarget = applySubst(target, subst)
      solveInvertible(currentTarget, ctx, subst, depth)
        .orElse(decomposeContext(ctx, subst, depth).flatMap { (newCtx, newSubst, wrapper) =>
          solve(target, newCtx, newSubst, depth).map { (t, s) => (wrapper(t), s) }
        })
        .orElse(solveSearch(currentTarget, ctx, subst, depth))

  private def solveInvertible(target: Prop, ctx: Context, subst: Subst, depth: Int): Option[(Term, Subst)] =
    if target == Prop.True then Some((Term.UnitTerm, subst))
    else target match
      case Prop.Arrow(from, to) =>
        val v = s"p$depth"
        solve(to, (v, from) :: ctx, subst, depth + 1).map { (b, s) => (Term.Abs(v, b), s) }
      case Prop.And(l, r) =>
        solve(l, ctx, subst, depth + 1).flatMap { (t1, s1) =>
          solve(r, ctx, s1, depth + 1).map { (t2, s2) => (Term.Pair(t1, t2), s2) }
        }
      case Prop.Forall(v, body) =>
        val xName = s"${v}_$depth"
        val instantiatedBody = substIndInProp(body, v, Ind.Var(xName))
        solve(instantiatedBody, ctx, subst, depth + 1).map { (b, s) => (Term.Abs(xName, b), s) }
      case _ => None

  private def solveSearch(target: Prop, ctx: Context, subst: Subst, depth: Int): Option[(Term, Subst)] =
    // Try Reflexivity
    val reflRes = target match
      case Prop.Eq(l, r) => unifyInd(l, r, subst).map(s => (Term.Refl(applySubst(l, s)), s))
      case _ => None
    if reflRes.isDefined then return reflRes

    // Try Axiom / False
    val axiomRes = ctx.view.flatMap { (name, prop) =>
      if prop == Prop.False then Some((Term.Absurd(Term.Var(name)), subst))
      else unifyProp(prop, target, subst).map(s => (Term.Var(name), s))
    }.headOption
    if axiomRes.isDefined then return axiomRes

    // Try Arrow Elim
    val arrowRes = ctx.view.flatMap { (name, prop) =>
      prop match
        case Prop.Arrow(from, to) =>
          unifyProp(to, target, subst).flatMap { s =>
            solve(from, ctx, s, depth + 1).map { (argP, sFinal) => (Term.App(Term.Var(name), argP), sFinal) }
          }
        case _ => None
    }.headOption
    if arrowRes.isDefined then return arrowRes

    // Try Forall Elim (Deep)
    val forallRes = ctx.view.flatMap { (name, prop) =>
      prop match
        case Prop.Forall(v, body) =>
          val m = freshMeta()
          val inst = substIndInProp(body, v, m)
          // Instantiate and try to prove the target using the instantiated formula
          solve(target, (s"inst_$depth", inst) :: ctx, subst, depth + 1).map { (t, s) =>
            // Since we added a hypothesis, we need to handle its proof term.
            // If the proof 't' uses 'inst_$depth', we replace it with 'Inst(name, m)'.
            (replaceVar(t, s"inst_$depth", Term.Inst(Term.Var(name), m)), s)
          }
        case _ => None
    }.headOption
    if forallRes.isDefined then return forallRes

    // Try Exists Intro
    val existsRes = target match
      case Prop.Exists(v, body) =>
        val m = freshMeta()
        solve(substIndInProp(body, v, m), ctx, subst, depth + 1).map { (p, s) => (Term.Witness(m, p), s) }
      case _ => None
    if existsRes.isDefined then return existsRes

    // Try Or Intro
    val orRes = target match
      case Prop.Or(l, r) =>
        solve(l, ctx, subst, depth + 1).map { (t, s) => (Term.Inl(t), s) }
          .orElse(solve(r, ctx, subst, depth + 1).map { (t, s) => (Term.Inr(t), s) })
      case _ => None
    if orRes.isDefined then return orRes

    // Try Rewrite
    val rewriteRes = ctx.view.flatMap { (name, prop) =>
      prop match
        case Prop.Eq(l, r) =>
          val r1 = rewriteProp(target, l, r)
          if r1 != target then solve(r1, ctx, subst, depth + 1).map { (p, s) => (Term.Rewrite(Term.Var(name), p), s) }
          else
            val r2 = rewriteProp(target, r, l)
            if r2 != target then solve(r2, ctx, subst, depth + 1).map { (p, s) => (Term.Rewrite(Term.Var(name), p), s) }
            else None
        case _ => None
    }.headOption
    if rewriteRes.isDefined then return rewriteRes

    None

  def decomposeContext(ctx: Context, subst: Subst, depth: Int): Option[(Context, Subst, Term => Term)] =
    ctx.zipWithIndex.collectFirst {
      case ((name, Prop.And(a, b)), i) =>
        val next = ctx.patch(i, List((s"fst($name)", a), (s"snd($name)", b)), 1)
        (next, subst, (t: Term) => t)
      case ((name, Prop.Exists(v, body)), i) =>
        val xName = s"${v}_sk_$depth"; val hName = s"h_sk_$depth"
        val next = ctx.patch(i, List((hName, substIndInProp(body, v, Ind.Var(xName)))), 1)
        (next, subst, (t: Term) => Term.Unpack(Term.Var(name), xName, hName, t))
    }

  def substIndInProp(p: Prop, v: String, t: Ind): Prop = p match
    case Prop.Pred(n, args) => Prop.Pred(n, args.map(a => if a.toString == v then t else a))
    case Prop.Eq(l, r) => Prop.Eq(if l.toString == v then t else l, if r.toString == v then t else r)
    case Prop.Arrow(f, to) => Prop.Arrow(substIndInProp(f, v, t), substIndInProp(to, v, t))
    case Prop.And(l, r) => Prop.And(substIndInProp(l, v, t), substIndInProp(r, v, t))
    case Prop.Or(l, r) => Prop.Or(substIndInProp(l, v, t), substIndInProp(r, v, t))
    case Prop.Forall(v2, b) => if v == v2 then p else Prop.Forall(v2, substIndInProp(b, v, t))
    case Prop.Exists(v2, b) => if v == v2 then p else Prop.Exists(v2, substIndInProp(b, v, t))
    case _ => p

  def replaceVar(t: Term, oldVar: String, newTerm: Term): Term = t match
    case Term.Var(n) if n == oldVar => newTerm
    case Term.Abs(p, b) => Term.Abs(p, if p == oldVar then b else replaceVar(b, oldVar, newTerm))
    case Term.App(f, a) => Term.App(replaceVar(f, oldVar, newTerm), replaceVar(a, oldVar, newTerm))
    case Term.Pair(a, b) => Term.Pair(replaceVar(a, oldVar, newTerm), replaceVar(b, oldVar, newTerm))
    case Term.Fst(t) => Term.Fst(replaceVar(t, oldVar, newTerm))
    case Term.Snd(t) => Term.Snd(replaceVar(t, oldVar, newTerm))
    case Term.Inl(t) => Term.Inl(replaceVar(t, oldVar, newTerm))
    case Term.Inr(t) => Term.Inr(replaceVar(t, oldVar, newTerm))
    case Term.Case(s, lv, lb, rv, rb) => 
      Term.Case(replaceVar(s, oldVar, newTerm), lv, if lv == oldVar then lb else replaceVar(lb, oldVar, newTerm), rv, if rv == oldVar then rb else replaceVar(rb, oldVar, newTerm))
    case Term.Witness(i, p) => Term.Witness(i, replaceVar(p, oldVar, newTerm))
    case Term.Unpack(ex, v, p, b) => 
      Term.Unpack(replaceVar(ex, oldVar, newTerm), v, p, if p == oldVar then b else replaceVar(b, oldVar, newTerm))
    case Term.Rewrite(eq, p) => Term.Rewrite(replaceVar(eq, oldVar, newTerm), replaceVar(p, oldVar, newTerm))
    case Term.Inst(f, a) => Term.Inst(replaceVar(f, oldVar, newTerm), a)
    case _ => t

  def rewriteProp(p: Prop, from: Ind, to: Ind): Prop = p match
    case Prop.Pred(n, args) => Prop.Pred(n, args.map(a => if a == from then to else a))
    case Prop.Eq(l, r) => Prop.Eq(if l == from then to else l, if r == from then to else r)
    case Prop.Arrow(f, t) => Prop.Arrow(rewriteProp(f, from, to), rewriteProp(t, from, to))
    case Prop.And(l, r) => Prop.And(rewriteProp(l, from, to), rewriteProp(r, from, to))
    case Prop.Or(l, r) => Prop.Or(rewriteProp(l, from, to), rewriteProp(r, from, to))
    case Prop.Forall(v, b) => Prop.Forall(v, rewriteProp(b, from, to))
    case Prop.Exists(v, b) => Prop.Exists(v, rewriteProp(b, from, to))
    case _ => p

// ==========================================
// 4. Parser & CLI
// ==========================================

object PropParser:
  class Scanner(val input: String):
    var pos = 0
    def peek: Char = if pos < input.length then input(pos) else 0.toChar
    def advance: Unit = if pos < input.length then pos += 1
    def eatWhitespace: Unit = while Character.isWhitespace(peek) do advance

  def parse(input: String): Prop =
    val s = new Scanner(input)
    val res = parseArrow(s)
    if s.peek != 0.toChar then throw new Exception(s"Unexpected char at ${s.pos}")
    res

  def parseArrow(s: Scanner): Prop =
    val left = parseOr(s)
    s.eatWhitespace
    if s.peek == '-' then { s.advance; consume(s, '>'); Prop.Arrow(left, parseArrow(s)) } else left

  def parseOr(s: Scanner): Prop =
    var left = parseAnd(s)
    while { s.eatWhitespace; s.peek == '|' } do { s.advance; left = Prop.Or(left, parseAnd(s)) }
    left

  def parseAnd(s: Scanner): Prop =
    var left = parseQuantifier(s)
    while { s.eatWhitespace; s.peek == '&' } do { s.advance; left = Prop.And(left, parseQuantifier(s)) }
    left

  def parseQuantifier(s: Scanner): Prop =
    s.eatWhitespace
    if s.input.startsWith("forall", s.pos)then{
      s.pos += 6
      val v = parseId(s)
      consume(s, '.')
      Prop.Forall(v, parseArrow(s))
    }
    else if s.input.startsWith("exists", s.pos)then{
      s.pos += 6
      val v = parseId(s)
      consume(s, '.')
      Prop.Exists(v, parseArrow(s))
    }
    else
      parseEq(s)

  def parseEq(s: Scanner): Prop =
    val left = parseAtom(s)
    s.eatWhitespace
    if s.peek == '=' then
      s.advance
      Prop.Eq(propToInd(left), propToInd(parseAtom(s)))
    else
      left

  def propToInd(p: Prop): Ind = p match
    case Prop.Pred(n, args) if args.isEmpty => Ind.Const(n)
    case _ => throw new RuntimeException("Invalid term:"+ p.toString)

  def parseAtom(s: Scanner): Prop =
    s.eatWhitespace
    if s.peek == '(' then { s.advance; val p = parseArrow(s); consume(s, ')'); p }
    else if s.peek == 'T' && !Character.isLetter(peekNext(s)) then { s.advance; Prop.True }
    else if s.peek == 'F' && !Character.isLetter(peekNext(s)) then { s.advance; Prop.False }
    else
      val name = parseId(s)
      s.eatWhitespace
      if s.peek == '(' then
        s.advance
        val args = mutable.ListBuffer[Ind]()
        if s.peek != ')' then
          args += parseInd(s)
          while s.peek == ',' do
            s.advance; args += parseInd(s)
        consume(s, ')'); Prop.Pred(name, args.toList)
      else Prop.Pred(name, Nil)

  def parseInd(s: Scanner): Ind = Ind.Const(parseId(s))
  def parseId(s: Scanner): String =
    s.eatWhitespace; val sb = new StringBuilder
    while Character.isLetterOrDigit(s.peek) || s.peek == '_' do { sb.append(s.peek); s.advance }
    if sb.isEmpty then throw new Exception("Expected ID"); sb.toString
  def peekNext(s: Scanner): Char =
    if s.pos + 1 < s.input.length then
      s.input(s.pos + 1)
    else
      0.toChar
  def consume(s: Scanner, c: Char): Unit = {
    s.eatWhitespace
    if s.peek == c then
      s.advance
    else
      throw new Exception(s"Expected $c")
  }

@main def runProver(args: String*): Unit =
  if args.nonEmpty then
    val s = scala.io.Source.fromFile(args.head)
    try
      for(
        l <- s.getLines
        if l.trim.nonEmpty && !l.startsWith("//")
      )processInput(l)
    finally
      s.close
  else
    println("=== Curry-Howard-Lambek Prover v2.1 (Refined) ===")
    while true do
      print("> ")
      val i = StdIn.readLine
      if (i == null || i == "exit") return else processInput(i)

def processInput(input: String): Unit =
  try
    val p = PropParser.parse(input)
    println(s"Goal: $p")
    Prover.prove(p) match
      case Some(t) => println(s"Proof: $t")
      case None => println("No proof found.")
  catch
    case e: Exception => println(s"Error: ${e.getMessage}")