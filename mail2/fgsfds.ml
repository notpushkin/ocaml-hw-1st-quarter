(*
 * fgsfds.ml
 * (l1::l2::l2::l3::l3::l3:: ...)
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

let fgsfds l =
  let lst = List.combine (range 1 (List.length l)) l
  in let fgsfds_inner l =
    let rec fgsfds_pair x = 
      if (first x) = 0 then
        []
      else
        [x] @ (fgsfds_pair ((fst x)-1, snd x))
    in List.concat (List.map (fgsfds_pair) l)
  in fgsfds_inner lst;;
