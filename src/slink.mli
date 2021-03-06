(* type 'a node *)
type 'a node = { value: 'a
               ; mutable next: 'a node option
               }

type 'a t

(* val value: 'a node -> 'a *)
val next: 'a node -> 'a node option
val empty : unit -> 'a t
val is_empty : 'a t -> bool
val from_node : 'a node -> 'a t
val from_value : 'a -> 'a t
val from_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
val length : 'a t -> int
val push : 'a -> 'a t -> 'a node
val append : 'a -> 'a t -> 'a node
val insert : 'a -> 'a node option -> 'a node option
val find_p : ('a -> bool) -> 'a t -> 'a node option
val find : 'a -> 'a t -> 'a node option
val remove_p : ('a -> bool) -> 'a t -> 'a node option
val remove : 'a -> 'a t -> 'a node option
val reverse : 'a t -> 'a t
