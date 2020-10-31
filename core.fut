-- Some Core functions I feel that futhark's prelude is in dire need of

-- elem returns true if a is in b, false otherwise
let elem 't (eq: t->t->bool) (a: t) (b: []t) : bool =
  loop
    u = false
    for x in b
      do (u || (x `eq` a))

-- unique returns the elements in arr that are unique (according to supplied equality function)
let unique 't (eq: t->t->bool) (arr: []t) : []t =
  loop a = [] for x in arr do
    if elem eq x a
      then a
      else concat a [x]
