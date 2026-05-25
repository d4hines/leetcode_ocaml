let%expect_test "" =
  print_endline "hello world";

  [%expect {|hello world|}]

type term = S | K | I | P of term * term
[@@deriving show { with_path = false }]

let rec rules t =
  match t with
  | P (I, x) -> rules x
  | P (P (K, x), _y) -> rules x
  | P (P (P (S, x), y), z) -> P (P (rules x, rules z), P (rules y, rules z))
  | P (x, y) -> P (rules x, rules y)
  (*    let x' = eval x in
      let y' = eval y in
      if x' = x && y' = y then P (x, y) else eval (P (x', y'))
    *)
  | x -> x

module type Evalable = sig
  type term
  val eq : term -> term -> bool
  val rules : term -> term
end

module Evaluate (E : Evalable) = struct 
  let eval t = 
      let t' = E.rules t in
end

let%expect_test "eval test" =
  (* Format.printf "%a" pp_term (eval I); *)
  let missing_case = P (P (I, I), P (I, I)) in
  Format.printf "%a" pp_term (eval missing_case);
  [%expect{| I |}]
