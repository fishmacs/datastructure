open Slink

let check_from_to_list = let open! QCheck in
  Test.make ~count:100 ~name:"to list should be equal to from list" (list small_nat) (fun l -> from_list l |> to_list = l)

let test_empty () =
  let lst = empty () in
  Alcotest.(check bool) "should be true" true (is_empty lst);
  Alcotest.(check int) "should be 0" 0 (length lst)

let test_length_one () = Alcotest.(check int) "should be 1" 1 (from_value 1 |> length)

let check_length_random = let open! QCheck in
  Test.make ~count:100 ~name:"lengths should be equal" (list small_nat) (fun l -> from_list l |> length = List.length l)

let test_push () =
  let lst = empty () in
  let node = push 1 lst in
  Alcotest.(check int) "node's value should be 1" 1 node.value;
  Alcotest.(check int) "lst's length should be 1" 1 (length lst);
  let node = push 2 lst in
  Alcotest.(check int) "node's value should be 2" 2 node.value;
  Alcotest.(check int) "lst's length should be 2" 2 (length lst)

let test_append () =
  let lst = empty () in
  let node = append 1 lst in
  Alcotest.(check int) "node's value should be 1" 1 node.value;
  Alcotest.(check int) "lst's length should be 1" 1 (length lst);
  let node = append 2 lst in
  Alcotest.(check int) "node's value should be 2" 2 node.value;
  Alcotest.(check int) "lst's length should be 2" 2 (length lst)

let nodetest = Alcotest.testable
    (fun ppf node -> match node with
       | None -> Fmt.pf ppf "None"
       | Some nd -> Fmt.pf ppf "%d" nd.value)
    (fun node1 node2 -> match (node1, node2) with
       | (None, None) -> true
       | (Some n1, Some n2) -> n1.value = n2.value
       | (_, _) -> false)

let test_find () =
  let lst = from_list [1; 2; 3; 4; 5] in
  Alcotest.(check nodetest) "Check found" (Some ({value=3; next=None})) (find 3 lst);
  Alcotest.(check nodetest) "Check not found" None (find 0 lst)

let test_remove () =
  let lst = from_list [1; 2; 3] in
  Alcotest.(check nodetest) "Check remove elem not existed" None (remove 0 lst);
  Alcotest.(check nodetest) "Check remove 2" (Some ({value=2; next=None})) (remove 2 lst);
  Alcotest.(check int) "Check list's length should be 2" 2 (length lst);
  Alcotest.(check nodetest) "Check remove 3" (Some ({value=3; next=None})) (remove 3 lst);
  Alcotest.(check int) "Check list's length should be 1" 1 (length lst);
  Alcotest.(check nodetest) "Check remove 1" (Some ({value=1; next=None})) (remove 1 lst);
  Alcotest.(check bool) "Check list's should be empty" true (is_empty lst)

let check_reverse = let open! QCheck in
  Test.make ~count:100 ~name:"reverse reverse should be original" (list small_nat) (fun l ->
      let lst = from_list l in
      if length lst > 1 then
        (reverse lst |> to_list <> l) &&
        (reverse lst |> to_list = l)
      else
        (reverse lst |> to_list = l) &&
        (reverse lst |> to_list = l))


let () =
  let suite = [ QCheck_alcotest.to_alcotest check_from_to_list
              ; "Check empty list and its length", `Quick, test_empty
              ; "Check list of 1 elem", `Quick, test_length_one
              ; QCheck_alcotest.to_alcotest check_length_random
              ; "Check push", `Quick, test_push
              ; "Check append", `Quick, test_append
              ; "Check find", `Quick, test_find
              ; "Check remove", `Quick, test_remove
              ; QCheck_alcotest.to_alcotest check_reverse
              ]
  in
  Alcotest.run "single linked list test" ["suite", suite]
