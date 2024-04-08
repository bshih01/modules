signature PQUEUE = sig
  type elem  (* element (a totally ordered type) *)
  val compare_elem : elem * elem -> order (* total order of elems *)

  type pqueue  (* Abstraction: a sorted list of 'elem' *)
  val empty  : pqueue                   (* the empty list *)
  val insert : elem * pqueue -> pqueue  (* insert (x, xs) == sort (x::xs) *)
  val isEmpty : pqueue -> bool          (* isEmpty xs == null xs *)
  exception Empty
  val deletemin : pqueue -> elem * pqueue
    (* deletemin [] = raises the Empty exception
       deletemin (x::xs) = (x, xs) *)

  (* cost model: insert and deletemin take at most logarithmic time;
                 isEmpty takes constant time*)
end
