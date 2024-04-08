signature BIGNUM = sig
   type bigint
   exception BadDivision           (* contract violation for sdiv *)

   val ofInt   : int -> bigint
   val negated : bigint -> bigint       (* "unary minus" *)
   val <+>     : bigint * bigint -> bigint
   val <->     : bigint * bigint -> bigint
   val <*>     : bigint * bigint -> bigint

   val compare : bigint * bigint -> order

   (* Contract for "Short division" sdiv:

      sdiv (n, d) nonnegative bigint n by nonnegative machine integer d 
      using short division. The return value is a record of the form
          
        { quotient = q, remainder = r}

      where q and r satisfy these laws:

        n == q /*/ ofInt d /+/ ofInt r
        0 <= r < d

      Given a negative n or d <= 0, sdiv (n, d) raises BadDivision.
   *)
   val sdiv    : bigint * int -> { quotient : bigint, remainder : int }


   (* toString n returns a string giving the natural 
      representation of n in the decimal system.  If n is
      negative, toString should use the conventional minus sign "-".

      And when toString returns a string containing two or more digits,
      the first digit must not be zero.
   *)
   val toString : bigint -> string



end
