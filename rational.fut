-- Rational number module

type ratio = {n: i64, d: i64}

let gcd (a: i64) (b: i64): i64 =
  (loop (x,y) = (a,b)
    while y > 0 do
      (y, x % y)
  ).0

let simplify ({n=a,d=b }: ratio) : ratio =
  {n = a / (gcd a b), d= b / (gcd a b) }
 

let to_flt ({n=a,d=b}): f32 = (f32.i64 a) / (f32.i64 b)

let eq (a: ratio) (b: ratio): bool =
  ((simplify a).n == (simplify b).n) &&
  ((simplify a).d == (simplify b).d)

let add (a:ratio) (b:ratio) : ratio =
  simplify {n= a.n*b.d+b.n*a.d, d= a.d*b.d}

let sub (a:ratio) (b:ratio) : ratio =
  simplify {n= a.n*b.d-b.n*a.d, d= a.d*b.d}

let mul ({n=a,d=b }: ratio) ({n=c,d=d }: ratio): ratio =
  simplify {n = a*c, d = b*d}

let div ({n=a,d=b }: ratio) ({n=c,d=d }: ratio): ratio =
  simplify {n = a*d, d = b*c}

-- Unit tests

entry simplify_test (a: [2]i64) : [2]i64 = 
[(simplify {n=a[0],d=a[1]}).n, (simplify {n=a[0],d=a[1]}).d]

-- Test of simplify function
-- ==
-- entry: simplify_test
-- input  {[1i64,8i64]}
-- output {[1i64,8i64]}
-- input  {[12i64,16i64]}
-- output {[3i64,4i64]}
