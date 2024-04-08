functor BignumFn(structure N : NATURAL) :> BIGNUM
  =
struct
  (* A bigint is either ZERO or POS n or NEG n, where n is a nat.
  *
  *  Invariants on our representation:
  *   In values of the form ZERO,
  *     - there are no invariants to maintain
  *   In values of the form POS n and NEG n,
  *     - n must be a nonzero natural number
  *)

  datatype bigint = POS of N.nat 
                  | NEG of N.nat 
                  | ZERO

  exception BadDivision

  fun ofInt i = 
    if i = 0 then 
      ZERO
    else if i > 0 then 
      POS (N.ofInt i)
    else 
      let
        val j = i + 1
        val negated = ~j
        val natPart = N.ofInt negated
      in
        NEG (N./+/ (natPart, N.ofInt 1))
      end

  fun negated (POS n) = NEG n
    | negated (NEG n) = POS n
    | negated ZERO    = ZERO

  infix 6 <+> <->
  infix 7 <*> sdiv

  fun (POS n) <+> (POS m) = POS (N./+/ (n, m))
    | (POS n) <+> (NEG m) = 
        if N.compare (n, m) = GREATER then 
          POS (N./-/ (n, m)) 
        else 
          NEG (N./-/ (m, n))
    | (NEG n) <+> (POS m) = 
        if N.compare (n, m) = GREATER then 
          NEG (N./-/ (n, m)) 
        else 
          POS (N./-/ (m, n))
    | (NEG n) <+> (NEG m) = NEG (N./+/ (n, m))
    | ZERO    <+> b       = b
    | a       <+> ZERO    = a

  fun a <-> b = a <+> negated b

  fun (POS n) <*> (POS m) = POS (N./*/ (n, m))
    | (POS n) <*> (NEG m) = NEG (N./*/ (n, m))
    | (NEG n) <*> (POS m) = NEG (N./*/ (n, m))
    | (NEG n) <*> (NEG m) = POS (N./*/ (n, m))
    | _       <*> _       = ZERO
  
  fun compare (POS n, POS m) = N.compare (n, m)
    | compare (NEG n, NEG m) = N.compare (m, n)
    | compare (NEG _, POS _) = LESS
    | compare (POS _, NEG _) = GREATER
    | compare (ZERO, ZERO) = EQUAL
    | compare (ZERO, _) = LESS
    | compare (_, ZERO) = GREATER
      
  fun x sdiv d =
    if d <= 0 orelse (compare (x, ZERO) = LESS) then
      raise BadDivision
    else
      (case x 
        of
          POS n =>
            let
              val { quotient = q, remainder = r } = N.sdiv (n, d)
            in
              { quotient = POS q, remainder = r }
            end
        | ZERO => { quotient = ZERO, remainder = 0 }
        | _    => raise BadDivision)

  fun toString ZERO = "0"
    | toString (POS n) = String.concat (map Int.toString (N.decimal n))
    | toString (NEG n) = "-" ^ String.concat (map Int.toString (N.decimal n))

end
