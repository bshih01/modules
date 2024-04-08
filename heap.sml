(* An implementation of an integer heap for testing purposes in Part B of the
 * 105 Modules assignment. *)
structure IntHeap :> PQUEUE where type elem = int =
struct
  type elem = int
  type height = int
  datatype t = EMPTY
             | NODE of elem * height * t * t
    (* invariant:
         elem in a node is not greater than the elems in its nonempty children
         left child is at least as high as right child
         height represents true height
     *)
  type pqueue = t
  fun le (x1, x2) = case Int.compare (x1, x2)
                      of GREATER => false
                       | _ => true
  val compare_elem = Int.compare

  fun height EMPTY = 0
    | height (NODE (_, h, _, _)) = h


  fun merge (EMPTY, q) = q
    | merge (q, EMPTY) = q
    | merge (q1 as NODE (x1, _, l1, r1), q2 as NODE (x2, _, _, _)) =
        if le (x1, x2) then
            let val (to_merge, to_stand) =
                  if height l1 > height q2 then (q2, l1) else (l1, q2)
                val newq1 = merge (r1, to_merge)
                val newq2 = to_stand
                val h1 = height newq1
                val h2 = height newq2
                val h = Int.max (h1, h2) + 1
                val (l, r) = if h1 > h2 then (newq1, newq2) else (newq2, newq1)
            in  NODE (x1, h, l, r)
            end
        else
            merge (q2, q1)
            
  val empty = EMPTY
  fun insert (x, EMPTY) = NODE (x, 1, EMPTY, EMPTY)
    | insert (x, q) = merge (insert(x, empty), q)

  fun isEmpty EMPTY = true
    | isEmpty (NODE _) = false

  exception Empty
  fun deletemin EMPTY = raise Empty
    | deletemin (NODE (x, _, q, q')) = (x, merge (q, q'))

end
