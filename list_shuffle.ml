(*
 * list_shuffle.ml
 * Version 2
 * 
 * Copyright 2012 Ale110 <pooh110andco@ya.ru>
 *
 * This program is free software. It comes without any warranty, to
 * the extent permitted by applicable law. You can redistribute it
 * and/or modify it under the terms of the Do What The Fuck You Want
 * To Public License, Version 2, as published by Sam Hocevar. See
 * http://sam.zoy.org/wtfpl/COPYING for more details.
 *)

Random.self_init();;

let rec range a b =
  if a > b then []
  else a :: range (a+1) b;;
  
let rm_nth l n =
  let lst = List.combine (range 0 ((List.length l)-1)) l
  in snd (List.split (List.remove_assoc n lst));;

let rand_list m n =
  let rec rand_list_inner lst nums =
    if nums = [] then
      lst
    else (
      let i = Random.int(List.length nums)
      in rand_list_inner (lst @ [List.nth nums i]) (rm_nth nums i)
    )
  in (rand_list_inner [] (range m n));;

let list_shuffle l =
  let lst = List.combine (rand_list 0 ((List.length l)-1)) l
  in snd (
    List.split (
      List.sort
        (fun x y -> (fst x) - (fst y))
        lst
    )
  );;
