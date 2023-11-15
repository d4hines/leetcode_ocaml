(* 224. Basic Calculator *)

(* type _ expr = *)
(*   | Plus : int expr * int expr -> int expr *)
(*   | Negation : int expr -> int expr *)
(*   | Int : int -> int expr *)
(**)
(* let rec eval : type a. a expr -> a = function *)
(*   | Plus (a, b) -> eval a + eval b *)
(*   | Negation a -> -eval a *)
(*   | Int i -> i *)

(* apply : (a -> b) t -> a t -> b t *)

(* bind : (a -> b t) -> a t -> b t *)

(* addition

   number := [0..9]+

   addition := expr + expr

   subtraction := expr - expr

   parens := (expr)

   negation := -expr

   expr := numer | addition | subtraction | parens *)
type expr =
  | Add of expr * expr
  | Subtract of expr * expr
  | Negate of expr
  | Int of int

type lexeme = LInt of int | LLParen | LRParen | LPlus | LMinus
[@@deriving show { with_path = false }]

let implode l = String.concat "" (List.map (String.make 1) l)
let is_digit c = match c with '0' .. '9' -> true | _ -> false

let is_whitespace c =
  match c with ' ' | '\t' | '\n' | '\r' -> true | _ -> false

let tokenize s =
  let rec go chars acc =
    match chars with
    | c :: chars when is_whitespace c -> go chars acc
    | c :: chars when not (is_digit c) ->
        let lexeme =
          match c with
          | '(' -> LLParen
          | ')' -> LRParen
          | '+' -> LPlus
          | '-' -> LMinus
          | _ -> failwith @@ Format.sprintf "Unexpected char '%c'" c
        in
        let result =
          if List.length acc > 0 then
            let int = List.rev acc |> implode |> int_of_string in
            [ LInt int; lexeme ]
          else [ lexeme ]
        in
        result @ go chars []
    | c :: chars -> go chars (c :: acc)
    | [] -> []
  in
  go (String.to_seq s |> List.of_seq) []

type token_list = lexeme list [@@deriving show]

(* let parse tokens =  *)
(*   let rec go tokens stack = match tokens, stack with *)
(*     | LPlus :: tokens, [Int i] -> *)
(*       Add (Int i, go tokens []) *)
(*  | LMinus ::  *)
(*   in assert false *)
let%expect_test "tokenize" =
  Format.printf "%a\n" pp_token_list (tokenize "123 + 4 + (2-1)");
  [%expect
    {|
    [(LInt 123); LPlus; (LInt 4); LPlus; LLParen; (LInt 2); LMinus; (LInt 1);
      LRParen] |}]
(**)
(* let parse s = *)
(*   let i = ref 0 in *)
(*   let take_until f = *)
(*     let chars = ref [] in *)
(*     let c = String.get s !i in *)
(*     while f c do *)
(*       chars := !chars @ [ c ]; *)
(*       incr i *)
(*     done; *)
(*     !chars *)
(*   in *)
(*   while !i < String.length s do *)
(*     let c = String.get s !i in *)
(*     let chars = ref [] in *)
(*     match c with *)
(*     | _ when is_digit c -> *)
(*         let number = take_until is_digit in *)
(*         failwith "help" *)
(*     | _ -> assert false *)
(*   done *)
(* open Opal *)
(* let parens = between (exactly '(') (exactly ')') *)
(* let int = many1 digit => implode % int_of_string *)
(* let add = exactly '+' >> return ( + ) *)
(* let sub = exactly '-' >> return ( - ) *)
(* let neg_op = exactly '-' >> return (fun x -> -x) *)
(* let rec expr input = chainl1 term (add <|> sub) input *)
(* and term input = (parens expr <|> neg <|> int) input *)
(* and neg input = (neg_op >>= fun f -> term => f) input *)
(* let%expect_test "calculator" = *)
(*   let eval_and_print str = *)
(*     let input = LazyStream.of_string str in *)
(*     let result = parse expr input |> Option.get in *)
(*     Format.printf "%s = %d\n" str result *)
(*   in *)
(*   eval_and_print "2+2"; *)
(*   eval_and_print "2+-2"; *)
(*   eval_and_print "2-2"; *)
(*   eval_and_print "-(1-2)"; *)
(*   eval_and_print "--(1-2)"; *)
(*   [%expect *)
(* {| *) (* 2+2 = 4 *) (* 2+-2 = 0 *) (* 2-2 = 0 *) (* -(1-2) = 1 *) (* --(1-2)
   = -1 |}] *)
