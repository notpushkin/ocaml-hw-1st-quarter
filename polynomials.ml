let rec poly_add l1 l2 =
  match l1, l2 with
    | [], [] -> []
    | _, [] -> l1
    | [], _ -> l2
    | x::ls1, y::ls2 -> (x+y) :: (poly_add ls1 ls2);;

let poly_sub l1 l2 =
  poly_add l1 (List.map (fun x -> -x) l2);;

let print_poly lst =
  List.iteri
  (fun i x ->
    print_int x;
    if i < (List.length l) then (
      print_string " * (x ^ ";
      print_int ((List.length lst) - i);
      print_string ") + "
    ) else ()
  )
  lst;;
