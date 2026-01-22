namespace Romanesco.Parsec

/-- パース結果 -/
inductive ParseResult (α : Type)
  | ok : α → Substring.Raw → ParseResult α
  | error : String → Substring.Raw → ParseResult α

/-- Parsec モナド -/
def Parsec (α : Type) := Substring.Raw → ParseResult α

instance : Monad Parsec where
  pure a := fun s => ParseResult.ok a s
  bind p f := fun s =>
    match p s with
    | ParseResult.ok a s' => f a s'
    | ParseResult.error e s' => ParseResult.error e s'

instance : Alternative Parsec where
  failure := fun s => ParseResult.error "failure" s
  orElse p1 p2 := fun s =>
    match p1 s with
    | ParseResult.ok a s' => ParseResult.ok a s'
    | ParseResult.error _ _ => p2 () s

def run (p : Parsec α) (s : String) : Except String α :=
  match p s.toRawSubstring with
  | ParseResult.ok a _ => Except.ok a
  | ParseResult.error e _ => Except.error e

def anyChar : Parsec Char := fun s =>
  match (s.front : Option Char) with
  | Option.none => ParseResult.error "eof" s
  | Option.some c => ParseResult.ok c (s.drop 1)

def satisfy (p : Char → Bool) : Parsec Char := do
  let c ← anyChar
  if p c then pure c else (fun s => ParseResult.error s!"unexpected character: {c}" s)

def pchar (c : Char) : Parsec Char := satisfy (· == c)

/-- 繰り返しのヘルパー -/
partial def many' (p : Parsec α) : Parsec (List α) :=
  (do let a ← p; let as ← many' p; pure (a :: as)) <|> pure []

partial def many1' (p : Parsec α) : Parsec (List α) :=
  do
  let a ← p
  let as ← many' p
  pure (a :: as)

def manyChars (p : Parsec Char) : Parsec String := do
  let cs ← many' p
  pure (String.ofList cs)

def many1Chars (p : Parsec Char) : Parsec String := do
  let cs ← many1' p
  pure (String.ofList cs)

def skipWs : Parsec Unit := do
  let _ ← many' (satisfy fun c => c == ' ' || c == '\t' || c == '\n' || c == '\r')
  pure ()

partial def skipAll : Parsec Unit := do
  let skipWsChar : Parsec Unit := do
    let _ ← satisfy (fun c => c == ' ' || c == '\t' || c == '\n' || c == '\r')
    pure ()
  let skipComment : Parsec Unit := do
    let _ ← pchar '#'
    let rec loop : Parsec Unit :=
      do
      let c ← anyChar
      if c == '\n' then pure () else loop
    loop
  let _ ← many' (skipWsChar <|> skipComment)
  pure ()

def identifier : Parsec String := do
  let first ← satisfy (fun c => c.isAlpha || c == '_')
  let rest ← manyChars (satisfy (fun c => c.isAlphanum || c == '_'))
  pure (String.ofList (first :: rest.toList))

def pnat : Parsec Nat := do
  let s ← many1Chars (satisfy (·.isDigit))
  match (s.toNat? : Option Nat) with
  | Option.some n => pure n
  | Option.none => (fun s => ParseResult.error "invalid number" s)

partial def pstring (matchStr : String) : Parsec String := do
  for c in matchStr.toList do
    let _ ← pchar c
  pure matchStr

end Romanesco.Parsec
