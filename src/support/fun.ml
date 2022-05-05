include Stdlib.Fun

let ( ++ ) f g x = f (g x)

let ( >> ) f g x = x |> f |> g

let[@inline] apply x f = f x

let flip f y x = f x y

let rec until f = if f () then until f else ()

let[@inline] through f x =
  f x;
  x

let after f x =
  f ();
  x

let[@inline] curry f x y = f (x, y)

let[@inline] uncurry f (x, y) = f x y
