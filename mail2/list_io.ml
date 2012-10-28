(** list_io.ml **
 * Version 2
 *
 * 2012-10-23  Ale110  <pooh110andco@ya.ru>
 * Fixed a typo in read_list
 * 
 * Copyright 2012 Alexander `Ale110` Pushkov <pooh110andco@ya.ru>
 * 
 * This program is free software. It comes without any warranty, to
 * the extent permitted by applicable law. You can redistribute it
 * and/or modify it under the terms of the Do What The Fuck You Want
 * To Public License, Version 2, as published by Sam Hocevar. See
 * http://sam.zoy.org/wtfpl/COPYING for more details. *)

let print_list f lst = 
  print_string "[";
  List.iter (fun x -> (f x; print_string "; ")) lst;
  print_string "]";;
  
let rec read_list f endel =
  let el = f() in
  if el = endel then
    []
  else
    [el] @ (read_list f endel);;
