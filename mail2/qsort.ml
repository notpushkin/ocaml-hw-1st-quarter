(** qsort.ml **
 * 
 * Copyright 2012 Alexander `Ale110` Pushkov <pooh110andco@ya.ru>
 * 
 * This program is free software. It comes without any warranty, to
 * the extent permitted by applicable law. You can redistribute it
 * and/or modify it under the terms of the Do What The Fuck You Want
 * To Public License, Version 2, as published by Sam Hocevar. See
 * http://sam.zoy.org/wtfpl/COPYING for more details. *)

let rec qsort lst =
  match lst with
    [] -> [] |
    el::ls ->
      qsort (List.filter (fun x -> x<el) ls)
      @ [el]
      @ qsort (List.filter (fun x -> x>el) ls);;
