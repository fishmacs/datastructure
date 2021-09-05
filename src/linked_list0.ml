let reverse : 'a linked_list -> 'a linked_list =
  fun l ->

    let rec reverse' : 'a linked_list -> 'a linked_list -> 'a linked_list =
      fun n1 n2 ->
        match n2 with
        | None -> n1
        | _    ->
           let n3 = get_next n2
           in
           ( set_next n2 n1 ; reverse' n2 n3 )
    in

    match l with
    | None -> None
    | _    -> reverse' None l
;;


let rec iterate : ('a -> unit) -> 'a linked_list -> unit =
  fun f l ->
    match l with
    | None -> ()
    | _    -> ( f (get_value l) ; iterate f (get_next l) )
;;


let print_list : ('a -> string) -> 'a linked_list -> unit =
  fun to_string l ->
    iterate (fun n -> print_string ((to_string n) ^ " ")) l
;;


let print_a_list_of_strings : string linked_list -> unit =
  fun l -> print_list (fun s -> s) l
;;


let print_a_list_of_ints : int linked_list -> unit =
  fun l -> print_list (fun i -> string_of_int i) l
;;



let main =
  let l1 = linked_list 1
  and l2 = linked_list 2
  and l3 = linked_list 3
  and l4 = linked_list 4
  and l5 = linked_list 5
  in

  let lists =
    [ ("l1", l1) ; ("l2", l2)
    ; ("l3", l3) ; ("l4", l4)
    ; ("l5", l5)
    ]
  in

  ( print_newline ()

  ; print_string ">>> Before linking the lists\n"
  ; List.iter (fun (s, l) ->
                   print_string (s ^ " : " )
                 ; print_a_list_of_ints l
                 ; print_newline ()
              )
              lists

  ; set_next l1 l2
  ; set_next l2 l3
  ; set_next l3 l4
  ; set_next l4 l5

  ; print_newline ()

  ; print_string ">>> After linking and before reversing the linked list\n"
  ; List.iter (fun (s, l) ->
                   print_string (s ^ " : " )
                 ; print_a_list_of_ints l
                 ; print_newline ()
              )
              lists

  ; print_newline ()

  ; print_string " The evaluation of `reverse l1´ computes the reversed\n\
                 \ list of l1 and the final result is equal to l5,\n\
                 \ the new head of the linked list.\n\n"

  ; let _ = reverse l1 in ()

  ; print_string ">>> After reversing the linked list\n"
  ; List.iter (fun (s, l) ->
                   print_string (s ^ " : " )
                 ; print_a_list_of_ints l
                 ; print_newline ()
              )
              lists

  ; print_newline ()
  )
;;



let () = main
