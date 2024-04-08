signature NATURAL = sig
   type nat

   exception Negative     (* the result of an operation is negative *)
   exception BadDivisor   (* divisor zero or negative *)

   val ofInt : int -> nat          (* could raise Negative *)
   val /+/   : nat * nat -> nat
   val /-/   : nat * nat -> nat    (* could raise Negative *)
   val /*/   : nat * nat -> nat

   (* Contract for "Short division" sdiv:

      sdiv (n, d) divides natural number n by digit d using short division. 
      The return value is a record of the form
          
        { quotient = q, remainder = r}

      where q and r satisfy these laws:

        n == q /*/ ofInt d /+/ ofInt r
        0 <= r < d

      If d <= 0 or d > base (where "base" is the hidden base selected to
      implement natural numbers), sdiv (n, d) raises BadDivisor.

   *)
   val sdiv  : nat * int -> { quotient : nat, remainder : int }
   
   (* compare (n1, n2) compares two natural numbers, following 
   *  these laws (we can't inspect the form of n1 and n2 since their
   *  representations are dictated by an implementation, not this interface):
   *
   *  compare (n1, n2) == EQUAL, when n1 == n2
   *  compare (n1, n2) == LESS, when n1 < n2
   *  compare (n1, n2) == GREATER, when n1 > n2
   *
   *)
   val compare : nat * nat -> order

   (* decimal n returns a list giving the natural decimal
      representation of n, most significant digit first.
      For example,  decimal (ofInt 123) = [1, 2, 3]
                    decimal (ofInt 0)   = [0]
      It must never return an empty list, and when it returns a 
      list of two or more digits, the first digit must not be zero.
   *)
   val decimal : nat -> int list

end
