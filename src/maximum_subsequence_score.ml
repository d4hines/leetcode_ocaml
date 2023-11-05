type int_list = int list [@@deriving show]
type int_list_list = int_list list [@@deriving show]

(* choose 1 from [] = [] ----

   choose 1 from [1] = [[1]] ---

   choose 1 from [1 ; 2] = [[1] [2]] choose 2 from [1 ; 2] = [[1; 2]] choose 2
   from [1; 2; 3] = [[1; 2]; [1; 3]; [2; 3]]

   choose 2 from [1; 2; 3; 4] = [[1; 2]; [1; 3]; [1; 4]; [2; 3]; [2; 4]; [3 ;
   4]] *)
let rec choose_k k l =
  match l with
  | _ when List.length l = k -> [ l ]
  | hd :: tl ->
      let choices_left = List.map (fun l -> hd :: l) (choose_k (k - 1) tl) in
      let choices_right = choose_k k tl in
      choices_left @ choices_right
  | [] -> []

let%expect_test "choose_k" =
  (* Format.printf "%a\n" pp_int_list_list (choose_k 1 [ 1; 2 ]); *)
  (* Format.printf "%a\n" pp_int_list_list (choose_k 2 [ 1; 2 ]); *)
  Format.printf "%a\n" pp_int_list_list (choose_k 2 [ 1; 2; 3; 4 ]);
  (* Format.printf "%a\n" pp_int_list_list (choose_k 3 [ 1; 3; 3; 2 ]); *)
  [%expect {|
    [[1; 2]; [1; 3]; [1; 4]; [2; 3]; [2; 4]; [3; 4]] |}]

let maximum_subsequence_score num1 num2 k =
  let choices = choose_k k @@ List.combine num1 num2 in
  List.fold_left
    (fun current_max choice ->
      let nums1, nums2 = List.split choice in
      let sum = List.fold_left ( + ) 0 nums1 in
      let min = List.fold_left min Int.max_int nums2 in
      max current_max (sum * min))
    0 choices

let%expect_test "choose_k" =
  Format.printf "%d\n"
  @@ maximum_subsequence_score [ 1; 3; 3; 2 ] [ 2; 1; 3; 4 ] 3;
  Format.printf "%d\n"
  @@ maximum_subsequence_score [ 4; 2; 3; 1; 1 ] [ 7; 5; 10; 9; 6 ] 1;

  [%expect {|
    12
    30 |}]

(* fn binary_search(v: Vec<i32>, t: i32) -> i32 { *)
(*     let mut l = 0; *)
(*     let mut r = v.len() - 1; *)
(*     while l <= r { *)
(*         let m = (l + r) / 2; *)
(*         if v[m] < t { *)
(*             l = m + 1 *)
(*         } else { *)
(*             if l + 1 == r { *)
(*                 return l; *)
(*             } else { *)
(*                 r = m - 1; *)
(*             } *)
(*         } *)
(*     } *)
(*     -1 *)
(* } *)

let arr = [| 0; 1; 3; 3; 5; 7; 10 |]

let binary_search v t =
  let l = ref 0 in
  let r = ref @@ (Array.length v - 1) in
  let exception Return of int in
  try
    while !l <= !r do
      let m = (!l + !r) / 2 in
      Format.printf "l %d, r %d, m %d\n%!" !l !r m;
      let v_m = Array.get v m in
      match Int.compare v_m t with
      | -1 -> l := m + 1
      | _ when !l = !r -> raise @@ Return !l
      | _ -> r := m
    done;
    -1
  with Return i -> i

let%expect_test "binary_search" =
  Format.printf "%d\n" (binary_search arr 2);
  Format.printf "%d\n" (binary_search arr 3);
  let arr = [| 0; 99 |] in
  Format.printf "%d\n" (binary_search arr 0);
  [%expect
    {|
    l 0, r 6, m 3
    l 0, r 3, m 1
    l 2, r 3, m 2
    l 2, r 2, m 2
    2
    l 0, r 6, m 3
    l 0, r 3, m 1
    l 2, r 3, m 2
    l 2, r 2, m 2
    2
    l 0, r 1, m 0
    l 0, r 0, m 0
    0 |}]
