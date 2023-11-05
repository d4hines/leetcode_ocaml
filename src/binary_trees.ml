type t = Empty | Node of { value : int; left : t; right : t }
[@@deriving show { with_path = false }]

module ListSet = Set.Make (struct
  type t = int list

  let compare = List.compare Int.compare
end)

type int_list = int list [@@deriving show]
type int_list_list = int_list list [@@deriving show]

let singleton x = Node { value = x; left = Empty; right = Empty }
let node x left right = Node { value = x; left; right }

(** Find all possible paths through the tree *)
let rec paths t =
  match t with
  | Empty -> ListSet.empty
  | Node { value; left; right } ->
      let paths_left = ListSet.map (List.append [ value ]) (paths left) in
      let paths_right = ListSet.map (List.append [ value ]) (paths right) in
      let this = ListSet.singleton [ value ] in
      ListSet.union this paths_left |> ListSet.union paths_right

let rec right_side_view t =
  match t with
  | Empty -> []
  | Node { value; left; right } ->
      let rec combine left right =
        match (left, right) with
        | _hdl :: left, hdr :: right -> hdr :: combine left right
        | [], right -> right
        | left, [] -> left
      in
      value :: combine (right_side_view left) (right_side_view right)

let%expect_test "right_side_view" =
  let tree =
    node 0
      (node 1 (singleton 2) (singleton 3))
      (node 3 (node 8 (singleton 6) (singleton 7)) (singleton 5))
  in
  Format.printf "%a" pp_int_list (right_side_view tree);
  [%expect {| [0; 3; 5; 7] |}]
