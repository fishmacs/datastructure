type 'a node = { value: 'a 
               ; mutable next: 'a node option
               }
type 'a head = { mutable next: 'a node option }

type 'a t = { head: 'a head; mutable last: 'a node option }

exception NotValue

let value n = n.value

let next n = n.next

let empty () =
  let head = { next=None } in
  { head; last=None }

let is_empty t = t.last = None

let length t =
  let rec length' (node: 'a node option) n = match node with
    | None -> n
    | Some node' -> length' node'.next (n + 1)
  in
  length' t.head.next 0

let push a t =
  let next = t.head.next in
  t.head.next <- Some { value=a; next };
  t

let append a t =
  let node = { value=a; next=None } in
  (match t.last with
   | Some node ->
       node.next <- None;
   | _ -> ()
  );
  t.last <- Some node;
  node

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
    | Some { value=v; next} as node ->
      if p v then node
      else find' p next
  in
  find' p t.head.next

let find e t = find_p (fun x -> e = x) t

let remove_p p t =
  let rec remove' p (prev: 'a node) = function
    | Some ({ value=v; next} as node) ->
      if p v then begin
        if t.last = Some node then
          t.last <- Some prev;
        prev.next <- next;
        node.next <- None;
        Some node
      end else remove' p node node.next
    | _ -> failwith "Head of linked list!"
  in
  match t.head.next with
  | None -> None
  | Some node as n ->
    if p node.value then (
      t.head.next <- n.next;
      if t.last = n then
        t.last <- None;
      node.next <- None;
      n
    ) else
      remove' p node node.next

let remove a t = remove_p (fun x -> x = a) t

let reverse t =
  let rec reverse' tail = function
    | None -> tail
    | Some node ->
      let next = node.next in
      node.next <- tail;
      reverse' (Some node) next
  in let hdn = reverse' None t.head.next
  in
  t.head.next <- hdn;
  t

let from_value a =
  let {head; _} = empty () in
  let node = { value=Val a; next=None } in
  head.next <- Some node;
  { head; last=node }

let to_list t =
  let rec to_list' accu = function
    | None -> accu
    | Some { value; next } ->
      to_list' (value :: accu) next
  in
  to_list' [] t.head.next |> List.rev

let from_list l =
  let rec from_list' finished = function
    | [] -> finished
    | hd::tl ->
      let _ = append hd finished in
      from_list' finished tl
  in
  from_list' (empty ()) l
