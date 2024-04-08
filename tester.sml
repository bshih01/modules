open GNatural

val () =
    Unit.checkExpectWith (Unit.listString Unit.intString) 
    "number conversion 2017"
    (fn () => decimal (ofInt 2017))
    [2, 0, 1, 7]

val () = Unit.reportWhenFailures ()