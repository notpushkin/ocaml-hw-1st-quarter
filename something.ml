(*
 * something.ml
 * 
 * Copyright 2012 Ale110 <pooh110andco@ya.ru>
 *
 * This program is free software. It comes without any warranty, to
 * the extent permitted by applicable law. You can redistribute it
 * and/or modify it under the terms of the Do What The Fuck You Want
 * To Public License, Version 2, as published by Sam Hocevar. See
 * http://sam.zoy.org/wtfpl/COPYING for more details.
 *)

let min x y = if x<y then x else y;;
let max x y = if x>y then x else y;;

let med3 x y z =
       if (x<y && y<z) || (z<y && y<x) then y
  else if (y<x && x<z) || (z<x && x<y) then x
  else z;;

let lll lst = 
  List.fold_left
    (fun mlen ls ->
      if (List.length ls) > mlen
        then (List.length ls)
        else mlen
    ) (-1) lst;;
