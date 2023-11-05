module Int_min = struct
  type t = int

  let pp fmt x = Format.fprintf fmt "%d" x
  let compare a b = Int.compare a b * -1
end

module Skew_min_heap = Skew_heap.Make (Int_min)

let kth_largest_element l k =
  let heap =
    List.fold_left
      (fun heap x -> Skew_min_heap.insert x heap)
      Skew_min_heap.empty l
  in
  let rec go heap i =
    match (Skew_min_heap.pop heap, i) with
    | None, _ -> None
    | Some (el, _heap), 1 -> Some el
    | Some (_el, heap), i -> go heap (i - 1)
  in
  go heap k

let%expect_test "ex 1" =
  let nums = [ 3; 2; 1; 5; 6; 4 ] in
  let k = 2 in
  let answer = kth_largest_element nums k |> Option.get in
  Format.printf "%d" answer;
  [%expect {|
    5 |}]

let%expect_test "ex 2" =
  let nums = [ 3; 2; 3; 1; 2; 4; 5; 5; 6 ] in
  let k = 4 in
  let answer = kth_largest_element nums k |> Option.get in
  Format.printf "%d" answer;
  [%expect {|
    4 |}]
