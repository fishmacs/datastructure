type 'a node = { value: 'a
               ; mutable next: 'a node option
               }

type 'a t = 'a node option ref

let value n = n.value
let next n = n.next

let empty () = ref None
let is_empty t = !t = None

let from_node node = ref (Some node)
let from_value a = ref (Some {value=a; next=None})

(* let insert_node node = function *)

let from_list l =
  let rec from_list' finished tail = function
    | [] -> finished
    | hd::tl ->
      let node = Some { value=hd; next=None } in
        match tail with
        | None -> from_list' (ref node) node tl
        | Some node' ->
          node'.next <- node;
          from_list' finished node tl
  in
  from_list' (empty ()) None l

let to_list t =
  let rec to_list' accu = function
    | None -> accu
    | Some { value; next } ->
      to_list' (value :: accu) next
  in
  to_list' [] !t |> List.rev

let length t =
  let rec length' node n = match node with
    | None -> n
    | Some node' -> length' node'.next (n + 1)
  in
  length' !t 0

let push a t =
  let node = { value=a; next=(!t) } in
  t := Some node;
  node

let append a t =
  let rec append' a node =
    match node.next with
    | None ->
      let node' = { value=a; next=None } in
      node.next <- Some node';
      node'
    | Some next -> append' a next
  in
  match !t with
  | None ->
    let head = { value=a; next=None } in
    t := Some head;
    head
  | Some node -> append' a node

let insert a = function
  | None -> None
  | Some node ->
    let next = node.next in
    let node' = Some { value=a; next } in
    node.next <- node';
    node'

let find_p p t =
  let rec find' p = function
    | None -> None
    | Some node ->
      if p node.value then Some node
      else find' p node.next
  in
  find' p !t

let find e t = find_p (fun x -> e = x) t

let remove_p p t =
  let rec remove' p prev = function
    | None -> None
    | Some node ->
      if p node.value then begin
        (match prev with
         | Some node' -> node'.next <- node.next;
         | _ -> t := node.next
        );
        node.next <- None;
        Some node
      end else remove' p (Some node) node.next
  in
  remove' p None !t

let remove a t = remove_p (fun x -> x = a) t

let reverse t =
  let rec reverse' tail node =
    let next = node.next in
    node.next <- tail;
    match next with
      | None -> Some node
      | Some node' -> reverse' (Some node) node'
  in match !t with
  | None -> t
  | Some node ->
    t := (reverse' None node);
    t
