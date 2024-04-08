functor PQSortFn(structure Q : PQUEUE) :> SORT where type elem = Q.elem =
  struct
    type elem    = Q.elem
    val compare  = Q.compare_elem
    fun sort lst =
      let
      fun insertAll [] pq      = pq
        | insertAll (x::xs) pq = insertAll xs (Q.insert (x, pq))

      fun extractAll pq = 
        if Q.isEmpty pq then 
          [] 
        else
          let 
            val (minElem, newPq) = Q.deletemin pq 
          in 
            minElem :: extractAll newPq 
          end
      in
        extractAll (insertAll lst Q.empty)
      end
  end
