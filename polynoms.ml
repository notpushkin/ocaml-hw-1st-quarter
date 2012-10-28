let rec poly_add l1 l2 =
  match l1, l2 with
    | [], [] -> []
    | _, [] -> l1
    | [], _ -> l2
    | x::ls1, y::ls2 -> (x+y) :: (poly_add ls1 ls2);;

let poly_sub l1 l2 =
  poly_add l1 (List.map (fun x -> -x) l2);;

let rec poly_mult l1 l2 =
  match l1 with
    | [] -> []
    | el::ls ->
      poly_add
        ((List.map (fun x -> el * x) l2) @ [0]) 
        (poly_mult ls l2);;
