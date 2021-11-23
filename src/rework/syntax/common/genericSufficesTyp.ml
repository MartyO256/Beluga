open Support

type 'a t =
  [ `exact of 'a
  | `infer of Location.t
  ]

let map f = function `exact x -> `exact (f x) | `infer loc -> `infer loc
