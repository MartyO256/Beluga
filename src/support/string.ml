include Stdlib.String

let unpack s =
  let n = length s in
  let rec unpack i return =
    if i < n
    then (
      let c = get s i in
      unpack (i + 1) (fun cs -> return (c :: cs)))
    else return []
  in
  unpack 0 Fun.id

let pack cs = concat "" (List.map (make 1) cs)
let drop n s = sub s n (length s - n)
