(*
 * lolwut.ml
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

let fact n = List.fold_left ( * ) 1 (range 1 n);;

let power x y = List.fold_left ( * ) 1
  (List.map (fun _ -> x) (range 1 y));;
