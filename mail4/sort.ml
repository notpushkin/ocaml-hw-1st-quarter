(*
 * sort.ml
 * 
 * Copyright 2012 Ale110 <pooh110andco@ya.ru>
 *
 * This program is free software. It comes without any warranty, to
 * the extent permitted by applicable law. You can redistribute it
 * and/or modify it under the terms of the Do What The Fuck You Want
 * To Public License, Version 2, as published by Sam Hocevar. See
 * http://sam.zoy.org/wtfpl/COPYING for more details.
 *)

(** Insertion sort *)

let rec insert el lst =
  match lst with
    | [] -> [el]
    | l1 :: ls ->
      if el <= l1 then el :: lst
      else l1 :: (insert el ls);;

let ins_sort lst = List.fold_right insert lst [];;

(** Quicksort *)

let rec qsort lst =
  match lst with
    [] -> [] |
    el::ls ->
      qsort (List.filter (fun x -> x<el) ls)
      @ [el]
      @ qsort (List.filter (fun x -> x>el) ls);;
