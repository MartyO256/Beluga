include Stdlib.Seq

let cons x next () = Cons (x, next)

let to_list =
  let rec go s return =
    match s () with
    | Nil -> return []
    | Cons (x, s) -> go s (fun xs -> return (x :: xs))
  in
  fun s -> go s Fun.id
