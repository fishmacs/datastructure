exception Empty

type 'a dbl_node =
  | Null
  | Node of { mutable value: 'a
            ; mutable prev: 'a dbl_node
            ; mutable next: 'a dbl_node
            }

let get_value = function
  | Null   -> raise Empty
  | Node r -> r.value

let set_value node value =
    match node with
    | Null   -> ()
    | Node r -> r.value <- value

let get_next = function
  | Null   -> raise Empty
  | Node r -> r.next

let set_next n1 n2 =
  match n1 with
  | Null   -> ()
  | Node r -> r.next <- n2

let get_prev = function
  | Null   -> raise Empty
  | Node r -> r.prev

let set_prev n1 n2 =
  match n1 with
  | Null   -> ()
  | Node r -> r.prev <- n2

type 'a dbl_linked_list = { mutable head: 'a dbl_node; mutable tail: 'a dbl_node }

let empty () = { head=Null; tail=Null }

let dbl_linked_list l = 
  let rec dbl_linked_list' finished = function
    | [] -> finished
    | hd::tl ->
      let node = Node {value=hd; prev=finished.tail; next=Null} in (
        match finished.head with
        | Null ->
          finished.head <- node;
        | _ ->
          set_next finished.tail node;
      );
      finished.tail <- node;
      dbl_linked_list' finished tl
  in
  dbl_linked_list' (empty ()) l
