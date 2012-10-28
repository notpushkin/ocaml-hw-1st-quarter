(* list-length.ml
 * 
 * Copyright 2012 Alexander `Ale110` Pushkov <pooh110andco@ya.ru>
 * 
 * This program is free software. It comes without any warranty, to
 * the extent permitted by applicable law. You can redistribute it
 * and/or modify it under the terms of the Do What The Fuck You Want
 * To Public License, Version 2, as published by Sam Hocevar. See
 * http://sam.zoy.org/wtfpl/COPYING for more details. *)

let list_length lst = 
  (* List.length lst;; *)
  (* List.fold_left (fun i -> i+1) 0 lst;; *)
  let rec list_length_inner i ls = 
    match ls with
      [] -> i |
      _::l -> list_length_inner (i+1) l
  in list_length_inner 0 lst;;
