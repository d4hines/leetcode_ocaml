module Char_map = Map.Make (Char)

type t = Node of { terminal : bool; children : t Char_map.t }

let empty = Node { terminal = false; children = Char_map.empty }

let insert s t =
  let rec go chars t =
    match (chars, t) with
    | [], Node { terminal = _; children } -> Node { terminal = true; children }
    | char :: chars, Node { terminal; children } -> (
        match Char_map.find_opt char children with
        | None ->
            Node
              {
                terminal;
                children = Char_map.add char (go chars empty) children;
              }
        | Some node ->
            let child = go chars node in
            Node { terminal; children = Char_map.add char child children })
  in
  go (String.to_seq s |> List.of_seq) t

let mem s t =
  let rec go chars t =
    match (chars, t) with
    | [], Node { terminal; children = _ } -> terminal
    | char :: chars, Node { terminal = _; children } -> (
        match Char_map.find_opt char children with
        | None -> false
        | Some trie -> go chars trie)
  in
  go (String.to_seq s |> List.of_seq) t

let%expect_test "mem" =
  let t = empty in
  Format.printf "%b\n" @@ mem "hello" t;
  Format.printf "%b\n" @@ mem "" t;
  let t = empty |> insert "hello" in
  Format.printf "%b\n" @@ mem "hello" t;
  Format.printf "%b\n" @@ mem "he" t;

  [%expect {|
    false
    false
    true
    false |}]

let starts_with s t =
  match (s, t) with
  | "", Node { terminal = true; children = _ } -> true
  | "", Node { terminal = false; children } -> Char_map.cardinal children > 0
  | _ ->
      let rec go chars t =
        match (chars, t) with
        | [], _ -> true
        | char :: chars, Node { terminal = _; children } -> (
            match Char_map.find_opt char children with
            | None -> false
            | Some node -> go chars node)
      in
      go (String.to_seq s |> List.of_seq) t

let%expect_test "starts_with" =
  let t = empty in
  Format.printf "empty starts with \"hello\": %b\n" @@ starts_with "hello" t;
  Format.printf "empty starts with \"\": %b\n" @@ starts_with "" t;
  Format.printf "\"\" starts with \"\": %b\n" @@ starts_with "" (insert "" t);
  Format.printf "\"\" starts with \"hello\": %b\n"
  @@ starts_with "hello" (insert "" t);
  let t = empty |> insert "hello" in
  Format.printf "\"hello\" starts with \"hello\": %b\n" @@ starts_with "hello" t;
  Format.printf "\"hello\" starts with \"he\": %b\n" @@ starts_with "he" t;
  Format.printf "\"hello\" starts with \"world\": %b\n" @@ starts_with "world" t;
  [%expect
    {|
    empty starts with "hello": false
    empty starts with "": false
    "" starts with "": true
    "" starts with "hello": false
    "hello" starts with "hello": true
    "hello" starts with "he": true
    "hello" starts with "world": false |}]

let entries (Node { terminal; children = _ } as node) =
  let rec get (Node { terminal = _; children }) =
    List.fold_left
      (fun results (char, (Node { terminal; children = _ } as node)) ->
        let s = String.make 1 char in
        let results = if terminal then [ s ] @ results else results in
        results @ List.map (fun result -> s ^ result) (get node))
      []
      (Char_map.bindings children)
  in
  let results = get node in
  if terminal then [ "" ] @ results else results

let%expect_test "entries" =
  let t =
    empty |> insert "" |> insert "hello" |> insert "world" |> insert "help"
  in
  List.iter
    (fun s ->
      if s = "" then print_endline "<empty string>" else print_endline s)
    (entries t);
  [%expect {|
    <empty string>
    help
    hello
    world |}]

let get_sub_trie prefix t =
  let rec go chars (Node { terminal = _; children } as node) =
    match chars with
    | [] -> Some node
    | char :: chars ->
        Option.bind (Char_map.find_opt char children) (fun node ->
            go chars node)
  in
  go (String.to_seq prefix |> List.of_seq) t

let entries_prefixed_by prefix t =
  get_sub_trie prefix t
  |> Option.map (fun subtrie ->
         List.map (fun s -> prefix ^ s) (entries subtrie))

let%expect_test "entries_prefixed_by" =
  let t =
    empty |> insert "" |> insert "hello" |> insert "world" |> insert "help"
  in
  let entries = Option.get @@ entries_prefixed_by "he" t in
  List.iter print_endline entries;
  [%expect {|
    help
    hello |}]
