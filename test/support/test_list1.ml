open OUnit2
open Support

let test_uncons (input, expected) _ =
  assert_equal
    ?printer:(Option.some (Pair.show Int.pp (List.pp Int.pp)))
    ?cmp:
      (Option.some (fun x y ->
           Pair.equal Int.equal (List.equal Int.equal) x y))
    expected (List1.uncons input)

let tests =
  "list1"
  >::: [ "uncons"
         >::: ([ (List1.from 1 [], (1, []))
               ; (List1.from 1 [ 2 ], (1, [ 2 ]))
               ; (List1.from 1 [ 2; 3 ], (1, [ 2; 3 ]))
               ]
              |> List.map Fun.(test_uncons >> OUnit2.test_case))
       ]
