let rec range a b =
  if a > b then []
  else a :: range (a+1) b;;

let harm n =
  List.map (fun x -> 1. /. (float_of_int x)) (range 1 n);;

let sum_harm n =
  List.fold_left (+.) 0. (harm n);;
