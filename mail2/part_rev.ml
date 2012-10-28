(*
 * part_rev.ml
 * 
 * Copyright 2012 Ale110 <pooh110andco@ya.ru>
 * 
 * TODO:
 * Обращение куска списка
 * n-й элемент с конца списка
 * Удалить n-й элемент списка
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
  
let first pair =
  match pair with
    (a, _) -> a;;

let second pair =
  match pair with
    (_, a) -> a;;

let print_list f lst = 
  print_string "[";
  List.iter (fun x -> (f x; print_string "; ")) lst;
  print_string "]";;

let part_rev l x y =
  let lst = List.combine (range 0 ((List.length l)-1)) l
  in
    let bef = List.filter (fun s -> (first s) < x) lst
    and ths = List.filter (fun s -> (first s) >= x && (first s) <= y) lst
    and aft = List.filter (fun s -> (first s) > y) lst
    in second (List.split (bef @ (List.rev ths) @ aft));;
  
  
  
  
  
  
  
  
(*_*)
