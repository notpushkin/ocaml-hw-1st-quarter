(*
 * mv_avg.ml
 * Simple moving average for a list
 *
 * Copyright 2012 Ale110 <pooh110andco@ya.ru>
 *
 * This program is free software. It comes without any warranty, to
 * the extent permitted by applicable law. You can redistribute it
 * and/or modify it under the terms of the Do What The Fuck You Want
 * To Public License, Version 2, as published by Sam Hocevar. See
 * http://sam.zoy.org/wtfpl/COPYING for more details.
 *)

let rec range a b =
  if a > b then []
  else a :: range (a+1) b;;

let list_avg lst =
  List.fold_left (+) 0 lst;;

(** Simple moving average for a list *)
let mov_avg len lst =
  List.map
    (fun i ->
      list_avg (List.map (fun n -> List.nth lst n) (range i (i+len-1)))
    )
    (range 0 ((List.length lst)-len));;
