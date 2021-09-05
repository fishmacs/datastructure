type 'a node

type 'a t

val empty: unit -> 'a t
val insert: 'a t -> 'a -> 'a node
val remove: 'a t -> 'a -> 'a node
