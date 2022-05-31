open Support

let level : int ref = ref 1

let ppf = Format.std_formatter

let print lvl x =
  let ppf =
    if lvl <= !level
    then ppf
    else Format.null_formatter
  in
  Format.fprintf ppf x
