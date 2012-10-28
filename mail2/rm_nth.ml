(*
 * rm_nth.ml
 * 
 * Copyright 2012 Ale110 <pooh110andco@ya.ru>
 * 
 * TODO:
 * ~ Обращение куска списка
 * ~ n-й элемент с конца списка
 * ~ Удалить n-й элемент списка
 * Переставить список по списку
 * Переставить список случайно
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
  
let second pair =
  match pair with
    (_, a) -> a;;

let rm_nth l n =
  let lst = List.combine (range 0 ((List.length l)-1)) l
  in second (List.split (List.remove_assoc n lst));;
  
  
  
  
  
  
  
  
(*_*)
