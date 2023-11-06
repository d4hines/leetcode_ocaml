(* 1268. Seach Suggestions System *)

module List = struct
  include List

  let rec take n l =
    match (n, l) with
    | _, [] | 0, _ -> []
    | i, hd :: tl -> hd :: take (i - 1) tl

  let%expect_test "take" =
    List.iter print_endline (take 0 []);
    List.iter print_endline (take 1 []);
    List.iter print_endline (take 1 [ "a"; "b"; "c" ]);
    print_endline "----";
    List.iter print_endline (take 2 [ "a"; "b"; "c" ]);
    print_endline "----";
    List.iter print_endline (take 3 [ "a"; "b"; "c" ]);

    [%expect
      {|
      a
      ----
      a
      b
      ----
      a
      b
      c |}]
end

let suggested_products (products : string list) (search_word : string) :
    string list list =
  let trie =
    List.fold_left (fun trie s -> Trie.insert s trie) Trie.empty products
  in
  let results = ref [] in
  for i = 1 to String.length search_word do
    let substr = String.sub search_word 0 i in
    match Trie.entries_prefixed_by substr trie with
    | None -> ()
    | Some entries ->
        let entries = List.sort String.compare entries |> List.take 3 in
        results := !results @ [ entries ]
  done;
  !results

type string_list = string list [@@deriving show]

let%expect_test "suggested_products test 1" =
  let products = [ "mobile"; "mouse"; "moneypot"; "monitor"; "mousepad" ] in
  List.iter
    (fun l -> Format.printf "%a\n" pp_string_list l)
    (suggested_products products "mouse");
  [%expect
    {|
    ["mobile"; "moneypot"; "monitor"]
    ["mobile"; "moneypot"; "monitor"]
    ["mouse";
                                                                        "mousepad"
                                                                        ]

    ["mouse"; "mousepad"]
    ["mouse"; "mousepad"] |}]

let%expect_test "suggested_products test 2" =
  let products = [ "havana" ] in
  List.iter
    (fun l -> Format.printf "%a\n" pp_string_list l)
    (suggested_products products "havana");
  [%expect
    {|
    ["havana"]
    ["havana"]
    ["havana"]
    ["havana"]
    ["havana"]
    ["havana"] |}]
