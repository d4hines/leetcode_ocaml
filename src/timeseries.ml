module Timeseries : sig
  type t
  type timestamp = int

  val add : timestamp -> int -> t -> t
  val sum_from_range : target:timestamp -> end_:timestamp -> t -> int option
  val max_from_range : target:timestamp -> end_:timestamp -> t -> int option
  val from_list : (timestamp * int) list -> t
end = struct
  type timestamp = int
  type t = (timestamp * int) list

  let sort t =
    List.sort (fun (time, _v) (time', _v') -> Int.compare time time') t

  let add n v t = sort ((n, v) :: t)

  (* TODO: Early return if range is obviously outside of range of the timeseies
  *)
  let sum_from_range ~target ~end_ t =
    List.fold_left
      (fun sum (timestamp, v) ->
        if timestamp >= target && timestamp <= end_ then
          match sum with Some sum -> Some (sum + v) | None -> Some v
        else sum)
      None t

  let max_from_range ~target ~end_ t =
    List.fold_left
      (fun m (timestamp, v) ->
        if timestamp >= target && timestamp <= end_ then
          match m with Some m -> Some (max m v) | None -> Some v
        else m)
      None t

  let from_list t = sort t

  let%expect_test "sum" =
    let t = [ (100, 1); (200, 2); (300, 3) ] in

    let s = sum_from_range ~target:150 ~end_:250 t |> Option.get in
    Format.printf "sum is %d\n" s;
    [%expect {| sum is 2 |}]

  let%expect_test "sum" =
    let t = [ (100, 1); (200, 2); (300, 3); (400, 2) ] in

    let s = max_from_range ~target:250 ~end_:1000 t |> Option.get in
    Format.printf "max is %d\n" s;
    [%expect {| max is 3 |}]
end

module Timeseries_fast = struct
  type timestamp = int [@@deriving show]

  type t =
    | Node of { k : timestamp; v : int; left : t; right : t; height : int }
    | Leaf
  [@@deriving show { with_path = false }]

  let height = function Leaf -> 0 | Node { height; _ } -> height

  let create left k v right =
    let hl = height left in
    let hr = height right in
    Node { left; k; v; right; height = (if hl >= hr then hl + 1 else hr + 1) }

  let empty = Leaf

  let balance left k v right =
    let hl = height left in
    let hr = height right in
    if hl > hr + 2 then
      match left with
      | Leaf -> invalid_arg "Map.bal"
      | Node { left = ll; k = lk; v = lv; right = lr; height = _ } -> (
          if height ll >= height lr then create ll lk lv (create lr k v right)
          else
            match lr with
            | Leaf -> invalid_arg "Map.bal"
            | Node { left = lrl; v = lrv; k = lrd; right = lrr; height = _ } ->
                create (create ll lk lv lrl) lrv lrd (create lrr k v right))
    else if hr > hl + 2 then
      match right with
      | Leaf -> invalid_arg "Map.bal"
      | Node { left = rl; k = rk; v = rv; right = rr; height = _ } -> (
          if height rr >= height rl then create (create left k v rl) rk rv rr
          else
            match rl with
            | Leaf -> invalid_arg "Map.bal"
            | Node { left = rll; k = rlk; v = rlv; right = rlr; height = _ } ->
                create (create left k v rll) rlk rlv (create rlr rk rv rr))
    else
      Node { left; k; v; right; height = (if hl >= hr then hl + 1 else hr + 1) }

  let rec add ~key ~value = function
    | Leaf -> Node { left = Leaf; k = key; v = value; right = Leaf; height = 1 }
    | Node { left; k; v; right; height } as m ->
        let c = Int.compare key k in
        if c = 0 then
          if value = v then m
          else Node { left; k = key; v = value; right; height }
        else if c < 0 then
          let ll = add ~key ~value left in
          if left == ll then m else balance ll k v right
        else
          let rr = add ~key ~value right in
          if right == rr then m else balance left k v rr

  (*  O((log n) + k) where k is start - end *)
  let rec iterate ~start ~end_ f t =
    match t with
    | Leaf -> ()
    | Node { k; v; left; right; height = _ } ->
        if k > end_ then
          (* If the current key is greater than end_, no need to traverse the right subtree *)
          iterate ~start ~end_ f left
        else if k < start then
          (* If the current key is less than start, no need to traverse the left subtree *)
          iterate ~start ~end_ f right
        else (
          (* If the current key is within the range, traverse left subtree, apply f, then traverse right subtree *)
          iterate ~start ~end_ f left;
          f k v;
          iterate ~start ~end_ f right)

  let add_from_range ~start ~end_ t =
    let sum = ref None in
    iterate ~start ~end_
      (fun _timestamp v ->
        match !sum with Some s -> sum := Some (v + s) | None -> sum := Some v)
      t;
    !sum

  let max_from_range ~start ~end_ t =
    let current_max = ref None in
    iterate ~start ~end_
      (fun _timestamp v ->
        match !current_max with
        | Some m -> current_max := Some (max m v)
        | None -> current_max := Some v)
      t;
    !current_max

  let%expect_test {|iterate|} =
    let t =
      empty |> add ~key:100 ~value:1 |> add ~key:200 ~value:2
      |> add ~key:400 ~value:3 |> add ~key:500 ~value:4 |> add ~key:600 ~value:5
      |> add ~key:700 ~value:3 |> add ~key:800 ~value:4 |> add ~key:900 ~value:5
    in

    Format.printf "%a\n" pp t;
    iterate ~start:150 ~end_:350
      (fun k v -> Format.printf "timestamp: %d, value: %d\n" k v)
      t;
    [%expect
      {|
      Node {k = 500; v = 4;
        left =
        Node {k = 200; v = 2;
          left = Node {k = 100; v = 1; left = Leaf; right = Leaf; height = 1};
          right = Node {k = 400; v = 3; left = Leaf; right = Leaf; height = 1};
          height = 2};
        right =
        Node {k = 700; v = 3;
          left = Node {k = 600; v = 5; left = Leaf; right = Leaf; height = 1};
          right =
          Node {k = 800; v = 4; left = Leaf;
            right = Node {k = 900; v = 5; left = Leaf; right = Leaf; height = 1};
            height = 2};
          height = 3};
        height = 4}
      timestamp: 200, value: 2 |}]

  let%expect_test {|add|} =
    let t =
      empty |> add ~key:100 ~value:1 |> add ~key:200 ~value:2
      |> add ~key:400 ~value:3 |> add ~key:500 ~value:2
    in
    let sum = add_from_range ~start:150 ~end_:600 t |> Option.get in
    Format.printf "%d\n" sum;
    [%expect {|
      7 |}]

  let%expect_test {|max|} =
    let t =
      empty |> add ~key:100 ~value:1 |> add ~key:200 ~value:2
      |> add ~key:400 ~value:3 |> add ~key:500 ~value:2
    in
    let sum = max_from_range ~start:150 ~end_:600 t |> Option.get in
    Format.printf "%d\n" sum;
    [%expect {|
      3 |}]
end
