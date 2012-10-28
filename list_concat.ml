(*
 * list_concat.ml
 * 
 * Copyright 2012 Ale110 <pooh110andco@ya.ru>
 *
 * This program is free software. It comes without any warranty, to
 * the extent permitted by applicable law. You can redistribute it
 * and/or modify it under the terms of the Do What The Fuck You Want
 * To Public License, Version 2, as published by Sam Hocevar. See
 * http://sam.zoy.org/wtfpl/COPYING for more details.
 *)

let rec list_concat lst = (* List.concat lst;; *)
  match lst with
    | [] -> []
    | el::ls -> el @ (list_concat lst);;

let list_concat' lst = List.fold_left (@) [] lst;;
