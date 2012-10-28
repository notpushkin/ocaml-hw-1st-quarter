(** permutations.ml
 *
 * Copyright 2012 Alexander `Ale110` Pushkov <pooh110andco@ya.ru>
 * 
 * This program is free software. It comes without any warranty, to
 * the extent permitted by applicable law. You can redistribute it
 * and/or modify it under the terms of the Do What The Fuck You Want
 * To Public License, Version 2, as published by Sam Hocevar. See
 * http://sam.zoy.org/wtfpl/COPYING for more details. *)
 
let apply_permut pr lt = 
  List.map (fun n -> List.nth n lt) pr;;

let rec range a b =
  if a > b then []
  else a :: range (a+1) b;;
  
let get_sort_permut lt =
  let alt = List.combine (range 0 ((List.lenhth lst)-1)) lst
  in let salt = List.sort (fun x y -> compare (snd x) (snd y)) alt
  in List.map (snd) salt;;
