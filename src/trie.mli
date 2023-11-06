type t

val empty : t
val insert : string -> t -> t
val mem : string -> t -> bool
val starts_with : string -> t -> bool
val entries : t -> string list
val get_sub_trie : string -> t -> t option
val entries_prefixed_by : string -> t -> string list option
