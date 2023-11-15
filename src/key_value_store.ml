module String_map = Map.Make (String)
module String_set = Set.Make (String)

type t = {
  mutable in_transaction : bool;
  mutable committed : string String_map.t;
  mutable uncommitted : string String_map.t;
  mutable deletes : String_set.t;
}
(* set A V begin delete A set A V' commit *)

let get { committed; uncommitted; in_transaction = _; deletes = _ } key =
  match String_map.find_opt key uncommitted with
  | Some x -> Some x
  | None -> String_map.find_opt key committed

let set t k v =
  if t.in_transaction then (
    t.uncommitted <- String_map.add k v t.uncommitted;
    t.deletes <- String_set.remove k t.deletes)
  else
    let updated = String_map.add k v t.committed in
    t.committed <- updated

let delete t k =
  if t.in_transaction then t.deletes <- String_set.add k t.deletes
  else
    let updated = String_map.remove k t.committed in
    t.committed <- updated

(* begin set A V commit begin delete A commit *)

(* let set = String_map.add *)
(* let delete = String_map.remove *)
let begin_ t = t.in_transaction <- true

let commit ({ committed; uncommitted; in_transaction = _; deletes  = _} as t) =
  let committed =
    String_map.merge
      (fun _key committed uncommitted ->
        match (committed, uncommitted) with
        | _, Some uncommitted -> Some uncommitted
        | committed, _ -> committed)
      committed uncommitted
  in
  let committed =
    String_set.fold
      (fun k committed -> String_map.remove k committed)
      t.deletes committed
  in
  t.committed <- committed;
  t.uncommitted <- String_map.empty;
  t.deletes <- String_set.empty;
  t.in_transaction <- false

(* let rollback t =  *)
(*   t.uncommitted <- String_set.empty; *)
(*   t.committed *)
