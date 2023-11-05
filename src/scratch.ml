module Tree = struct
  type t = Empty | Node of { value : int; left : t; right : t }
  [@@deriving show { with_path = false }]

  module ListSet = Set.Make (struct
    type t = int list

    let compare = List.compare Int.compare
  end)

  let rec paths t =
    match t with
    | Empty -> ListSet.empty
    | Node { value; left; right } ->
        let paths_left = ListSet.map (List.append [ value ]) (paths left) in
        let paths_right = ListSet.map (List.append [ value ]) (paths right) in
        let this = ListSet.singleton [ value ] in
        ListSet.union this paths_left |> ListSet.union paths_right

  (* 0 1 2 3 4 *)

  let tree =
    Node
      {
        value = 0;
        left = Node { value = 1; left = Empty; right = Empty };
        right =
          Node
            {
              value = 2;
              left = Node { value = 3; left = Empty; right = Empty };
              right = Node { value = 4; left = Empty; right = Empty };
            };
      }

  let%expect_test "" =
    Format.printf "%a" pp tree;
    [%expect
      {|
      Node {value = 0; left = Node {value = 1; left = Empty; right = Empty};
        right =
        Node {value = 2; left = Node {value = 3; left = Empty; right = Empty};
          right = Node {value = 4; left = Empty; right = Empty}}} |}]
  (* 0 1 3 2 *)

  (* 5 --- 5 1 3 --- 5 1 3 10 *)
  let tree2 =
    Node
      {
        value = 0;
        left =
          Node
            {
              value = 1;
              left = Node { value = 2; left = Empty; right = Empty };
              right = Empty;
            };
        right = Node { value = 3; left = Empty; right = Empty };
      }

  let path_sum t target_sum =
    ListSet.filter (fun l -> List.fold_left ( + ) 0 l = target_sum) (paths t)
    |> ListSet.cardinal

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

  let rec get_values_at_level t n =
    match (t, n) with
    | _, 0 -> []
    | Empty, _ -> []
    | Node { value; left = _; right = _ }, 1 -> [ value ]
    | Node { value = _; left; right }, n ->
        let next = n - 1 in
        get_values_at_level left next @ get_values_at_level right next

  let rec max_depth t =
    match t with
    | Empty -> 0
    | Node { value = _; left; right } ->
        1 + max (max_depth left) (max_depth right)

  let max_level_sum t =
    let rec go n =
      match n with 0 -> [] | n -> get_values_at_level t n :: go (n - 1)
    in
    let all_levels = go (max_depth t) in
    List.fold_left
      (fun max' l ->
        let sum = List.fold_left ( + ) 0 l in
        max max' sum)
      0 all_levels

  let rec search_bst t v =
    match t with
    | Empty -> None
    | Node { value; left = _; right = _ } as node when value = v -> Some node
    | Node { value = _; left; right } -> (
        match (search_bst left v, search_bst right v) with
        | Some node, _ -> Some node
        | _, Some node -> Some node
        | None, None -> None)
end

module type ORD = sig
  include Set.OrderedType

  val pp : Format.formatter -> t -> unit
end
