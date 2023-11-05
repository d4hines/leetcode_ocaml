module type ORD = sig
  include Set.OrderedType

  val pp : Format.formatter -> t -> unit
end

module Make =
functor
  (Ord : ORD)
  ->
  struct
    type t = Empty | Node of { v : Ord.t; left : t; right : t }
    [@@deriving show { with_path = false }]

    let empty = Empty
    let singleton v = Node { v; left = Empty; right = Empty }

    let rec union t1 t2 =
      match (t1, t2) with
      | Empty, t2 -> t2
      | t1, Empty -> t1
      | ( (Node { v = v1; left = l1; right = r1 } as t1),
          Node { v = v2; left = l2; right = r2 } ) ->
          if Ord.compare v1 v2 = -1 then
            Node { v = v1; left = union t2 r1; right = l1 }
          else Node { v = v2; left = union t1 r2; right = l2 }

    let insert x t = union (singleton x) t

    let pop t =
      match t with
      | Empty -> None
      | Node { v; left; right } ->
          let t = union left right in
          Some (v, t)
  end

module Int = struct
  include Int

  let pp fmt x = Format.fprintf fmt "%d" x
end

module Skew_heap_int = Make (Int)

let%expect_test "Skew_heap" =
  let open Skew_heap_int in
  let show t = Format.printf "%a\n%!" pp t in
  let a =
    Node
      {
        v = 1;
        left = Node { v = 4; left = singleton 63; right = singleton 24 };
        right = Node { v = 23; left = singleton 44; right = singleton 28 };
      }
  in
  let b =
    Node
      {
        v = 13;
        left = Node { v = 17; left = singleton 57; right = singleton 49 };
        right = Node { v = 99; left = singleton 105; right = singleton 201 };
      }
  in
  show a;
  print_endline "====";
  show b;
  print_endline "====";
  show @@ union a b;
  [%expect
    {|
    Node {v = 1;
      left =
      Node {v = 4; left = Node {v = 63; left = Empty; right = Empty};
        right = Node {v = 24; left = Empty; right = Empty}};
      right =
      Node {v = 23; left = Node {v = 44; left = Empty; right = Empty};
        right = Node {v = 28; left = Empty; right = Empty}}}
    ====
    Node {v = 13;
      left =
      Node {v = 17; left = Node {v = 57; left = Empty; right = Empty};
        right = Node {v = 49; left = Empty; right = Empty}};
      right =
      Node {v = 99; left = Node {v = 105; left = Empty; right = Empty};
        right = Node {v = 201; left = Empty; right = Empty}}}
    ====
    Node {v = 1;
      left =
      Node {v = 13;
        left =
        Node {v = 23;
          left =
          Node {v = 28;
            left =
            Node {v = 99; left = Node {v = 105; left = Empty; right = Empty};
              right = Node {v = 201; left = Empty; right = Empty}};
            right = Empty};
          right = Node {v = 44; left = Empty; right = Empty}};
        right =
        Node {v = 17; left = Node {v = 57; left = Empty; right = Empty};
          right = Node {v = 49; left = Empty; right = Empty}}};
      right =
      Node {v = 4; left = Node {v = 63; left = Empty; right = Empty};
        right = Node {v = 24; left = Empty; right = Empty}}} |}]
(* This attempt isn't going well :(

   module Min_heap = functor (Ord : ORD) -> struct type elt = Ord.t

   type tree = Empty | Node of { v : Ord.t; left : tree; right : tree }
   [@@deriving show]

   type path = Left | Right [@@deriving show] type t = { next : path list; tree
   : tree } [@@deriving show]

   let empty = { next = []; tree = Empty } let singleton x = Node { v = x; left
   = Empty; right = Empty }

   let rec insert_at_end x { next; tree } i = match tree with | Empty -> { next
   = next @ [ Left ]; tree = singleton x } | Node { v; left; right } -> let
   next_i = List.nth next i in match (i, next_i) with | 0 Left -> | Right { next
   = [ Right ]; tree = Node { v; left = singleton x; right = Empty }; } | Right
   :: [], Node { v; left; right = Empty } -> { next = next @ [ Left ]; tree =
   Node { v; left; right = singleton x }; } | hd :: tl, Node { v; left; right }
   -> ( match hd with | Left -> let { next; tree } = insert_at_end x { next =
   tl; tree = left } in { next = [ hd ] @ next; tree = Node { v; left = tree;
   right } } | Right -> let { next; tree } = insert_at_end x { next = tl; tree =
   right } in { next = [ hd ] @ next; tree = Node { v; left; right = tree } }) |
   _ -> assert false

   let push _x _t = assert false let pop _t = assert false

   let peek { next = _; tree } = match tree with Empty -> None | Node { v; _ }
   -> Some v

   module Internal = struct let insert_at_end = insert_at_end end end

   module Min_heap_int = Min_heap (struct type t = int

   let compare = Int.compare let pp fmt x = Format.fprintf fmt "%d" x end)

   let show_tree t = Format.printf "%a\n%!" Min_heap_int.pp t

   let%expect_test "insert_at_end" = let tree = Min_heap_int.empty |>
   Min_heap_int.insert_at_end 100 in show_tree tree; print_endline "---"; let
   tree = tree |> Min_heap_int.insert_at_end 19 in show_tree tree; print_endline
   "---"; let tree = tree |> Min_heap_int.insert_at_end 36 in show_tree tree;
   print_endline "---"; let tree = tree |> Min_heap_int.insert_at_end 17 |>
   Min_heap_int.insert_at_end 12 |> Min_heap_int.insert_at_end 25 in show_tree
   tree;

   [%expect {| { Lib.Min_heap.next = [Lib.Min_heap.Left]; tree =
   Lib.Min_heap.Node {v = 100; left = Lib.Min_heap.Empty; right =
   Lib.Min_heap.Empty} } --- { Lib.Min_heap.next = [Lib.Min_heap.Right]; tree =
   Lib.Min_heap.Node {v = 100; left = Lib.Min_heap.Node {v = 19; left =
   Lib.Min_heap.Empty; right = Lib.Min_heap.Empty}; right = Lib.Min_heap.Empty}
   } --- { Lib.Min_heap.next = [Lib.Min_heap.Right; Lib.Min_heap.Left]; tree =
   Lib.Min_heap.Node {v = 100; left = Lib.Min_heap.Node {v = 19; left =
   Lib.Min_heap.Empty; right = Lib.Min_heap.Empty}; right = Lib.Min_heap.Node {v
   = 36; left = Lib.Min_heap.Empty; right = Lib.Min_heap.Empty}} } --- {
   Lib.Min_heap.next = [Lib.Min_heap.Right; Lib.Min_heap.Right;
   Lib.Min_heap.Right]; tree = Lib.Min_heap.Node {v = 100; left =
   Lib.Min_heap.Node {v = 19; left = Lib.Min_heap.Empty; right =
   Lib.Min_heap.Empty}; right = Lib.Min_heap.Node {v = 36; left =
   Lib.Min_heap.Node {v = 17; left = Lib.Min_heap.Empty; right =
   Lib.Min_heap.Empty}; right = Lib.Min_heap.Node {v = 12; left =
   Lib.Min_heap.Node {v = 25; left = Lib.Min_heap.Empty; right =
   Lib.Min_heap.Empty}; right = Lib.Min_heap.Empty}}} } |}] *)
