type interval_list = (int * int) list [@@deriving show]

let erase_overlapping_intervals (intervals : (int * int) list) =
  let intervals =
    List.sort
      (fun (a, a') (b, b') ->
        match Int.compare a' b' with 0 -> Int.compare a b | x -> x)
      intervals
  in
  Format.printf "%a\n" pp_interval_list intervals;
  let rec go acc l =
    match (acc, l) with
    | _, [] -> acc
    | (_a, a') :: _acc_tl, (b, b') :: l_tl ->
        Format.printf "comparing (%d, %d) with (%d, %d)\n" _a a' b b';
        if b < a' then
          let () = Format.printf "throwing away (%d, %d)\n" b b' in
          go acc l_tl
        else
          let () = Format.printf "adding (%d, %d)\n" b b' in
          go ((b, b') :: acc) l_tl
    | [], (b, b') :: l_tl -> go [ (b, b') ] l_tl
  in
  go [] intervals |> List.rev

let%expect_test "example 1" =
  Format.printf "%a" pp_interval_list
    (erase_overlapping_intervals [ (1, 2); (2, 3); (3, 4); (1, 3) ]);
  [%expect
    {|
    [(1, 2); (1, 3); (2, 3); (3, 4)]
    comparing (1, 2) with (1, 3)
    throwing away (1, 3)
    comparing (1, 2) with (2, 3)
    adding (2, 3)
    comparing (2, 3) with (3, 4)
    adding (3, 4)

    [(1, 2); (2, 3); (3, 4)] |}]

let%expect_test "example 2" =
  Format.printf "%a" pp_interval_list
    (erase_overlapping_intervals [ (1, 2); (1, 2); (1, 2) ]);
  [%expect
    {|
    [(1, 2); (1, 2); (1, 2)]
    comparing (1, 2) with (1, 2)
    throwing away (1, 2)
    comparing (1, 2) with (1, 2)
    throwing away (1, 2)

    [(1, 2)] |}]

let%expect_test "example 3" =
  Format.printf "%a" pp_interval_list
    (erase_overlapping_intervals [ (1, 2); (2, 3) ]);
  [%expect
    {|
    [(1, 2); (2, 3)]
    comparing (1, 2) with (2, 3)
    adding (2, 3)
    [(1, 2); (2, 3)] |}]
