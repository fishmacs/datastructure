exception Empty

type 'a linked_list =
  | Null
  | Node of { mutable value: 'a
            ; mutable next: 'a linked_list
            }

let linked_list : 'a -> 'a linked_list =
  fun x ->
    Node { value = x
         ; next  = Null
         }

let get_value : 'a linked_list -> 'a =
  function
  | Null   -> raise Empty
  | Node r -> r.value

let set_value : 'a -> 'a linked_list -> unit =
  fun value node ->
    match node with
    | Null   -> ()
    | Node r -> r.value <- value

let get_next : 'a linked_list -> 'a linked_list =
  function
  | Null   -> raise Empty
  | Node r -> r.next

let set_next : 'a linked_list -> 'a linked_list -> unit =
  fun n1 n2 ->
    match n1 with
    | Null   -> ()
    | Node r -> r.next <- n2

let rec reverse: 'a linked_list -> 'a linked_list =
  function
  | Null -> Null
  | Node r as node ->
    match r.next with
    | Null -> node
    | Node {value; next=next'} as next ->
      let rnode = reverse next in
      (* next' can not be assigned, but can be as a ref *)
      next' <- node;
      r.next <- Null;
      rnode

let l1 = linked_list 1;;
let l2 = linked_list 2;;
let l3 = linked_list 3;;
let l4 = linked_list 4;;
let l5 = linked_list 5;;
set_next l1 l2;;
set_next l2 l3;;
set_next l3 l4;;
set_next l4 l5;;


let reverse_list: 'a list -> 'alist =
  fun l ->
    let rec reverse l accu =
      match l with
      | [] -> accu
      | hd::tl -> reverse tl (hd::accu)
    in
    reverse  l []

  (* | Node {value; next=Node {value=v'; next=next'} as node'} as node ->
   *   let rnode = reverse next' in
   *   rnode.next <- node'.next;
   *   node'.next <- node;
   *   rnode
   * | Node {value; next=Node {value=v'; next=next_node'} as next_node} as node ->
   *   let rnode = reverse next_node in begin
   *     next_node' <- node;
   *     node' <- Null;
   *     end
   *       rnode *)
      
