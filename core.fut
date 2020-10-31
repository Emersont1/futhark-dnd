-- Some Core functions I feel that futhark's prelude is in dire need of

-- elem returns if there exists an x in b, such that eq(a,b) == true
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


-- Unit tests

entry int_elem_test (a: i32) (b: []i32) : bool = elem (==) a b

-- Test of elem with i32 type
-- ==
-- entry: int_elem_test
-- input  {1 [1, 2, 3, 4]}
-- output {true}
-- input  {5 [1, 2, 3, 4]}
-- output {false}

entry int_unique_test (a: []i32) : []i32 = unique (==) a

-- Test of unique with i32 type
-- ==
-- entry: int_unique_test
-- input  {[1,1]}
-- output {[1]}
-- input  {[1,2,3,4,5,6,7]}
-- output {[1,2,3,4,5,6,7]}
