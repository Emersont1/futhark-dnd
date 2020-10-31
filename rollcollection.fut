import "core"
module rational = import "rational"

type roll 't = {value: t, prob: rational.ratio}

let dice (n: i64): []roll i64 =
  map (\ x -> {value=x, prob={n=1, d=n}}) (1...n)

let apply 't 'u 'v (f: (t -> u -> v)) (a: roll t) (b: roll u): roll v =
  {
    value=f a.value b.value,
    prob=a.prob `rational.mul` b.prob
  }

let combine 't 'u 'v (f: (t -> u -> v)) (a: []roll t) (b: []roll u): []roll v =
  flatten (map (\i -> map (\j -> apply f i j) b) a)

let collapse 't (a: []roll t) (eq: t->t->bool): []roll t = 
  map 
    (\x -> {value=x, prob=(
      foldr rational.add {d=1, n=0} (map 
                                       (\x-> x.prob)
                                       (filter (\u -> eq x u.value) a))
      )})
      (unique eq (map (\x->x.value) a))

let visualise 't (a : []roll t): [](t, f32) = 
  map (\x -> (x.value, rational.to_flt x.prob)) a

