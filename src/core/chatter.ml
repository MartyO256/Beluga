let level : int ref = ref 1

let print lvl =
  let ppf =
    if lvl <= !level
    then Format.std_formatter
    else Support.Fmt.null_formatter
  in
  fun x -> Format.fprintf ppf x
