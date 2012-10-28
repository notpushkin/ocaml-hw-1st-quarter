let list_unique lst = 
  let rec list_unique_inner buf lst =
    match lst with
      | [] -> buf
      | el :: ls when not(List.exists ((=) el) buf) ->
        list_unique_inner (el::buf) ls
      | _ :: ls ->
        list_unique_inner buf ls
  in List.rev (list_unique_inner [] lst);;

let list_union l1 l2 = list_unique (l1 @ l2);;

let list_complement l1 l2 = 
  list_unique (List.filter (fun el -> not(List.exists ((=) el) l2)) l1);;

let list_intersect l1 l2 = list_complement l1 (list_complement l1 l2);;

let list_symm_diff l1 l2 =
  list_union (list_complement l1 l2) (list_complement l2 l1);;

let list_cart_product l1 l2 =
  let x = List.concat (List.map (fun x -> l1) (l2))
  and y = List.concat (
    List.map (fun y ->(List.map (fun x -> y) l1)) l2
  )
  in List.combine x y;;
