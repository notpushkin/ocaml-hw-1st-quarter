(** polynomials.ml **
 * Version 2
 *
 * 2012-10-28  Ale110  <pooh110andco@ya.ru>
 * Added print_poly andcalc_poly functions.
 * 
 * Copyright 2012 Alexander `Ale110` Pushkov <pooh110andco@ya.ru>
 * 
 * This program is free software. It comes without any warranty, to
 * the extent permitted by applicable law. You can redistribute it
 * and/or modify it under the terms of the Do What The Fuck You Want
 * To Public License, Version 2, as published by Sam Hocevar. See
 * http://sam.zoy.org/wtfpl/COPYING for more details. *)

(* from Ocaml 4.00.1 *)
(* val mapi : (int -> 'a -> 'b) -> 'a list -> 'b *)
let rec mapi i f = function
    [] -> []
  | a::l -> let r = f i a in r :: mapi (i + 1) f l;;

let mapi f l = mapi 0 f l;;
let iteri f l = mapi f l;;
(* /from Ocaml *)

(** power funcs *)
let (^^) x y = (float_of_int x) ** (float_of_int y);;
let (^.^) x y = x ** (float_of_int y);;
let (^^.) x y = (float_of_int x) ** y;;

let rec poly_add l1 l2 =
  match l1, l2 with
    | [], [] -> []
    | _, [] -> l1
    | [], _ -> l2
    | x::ls1, y::ls2 -> (x+y) :: (poly_add ls1 ls2);;

let poly_sub l1 l2 =
  poly_add l1 (List.map (fun x -> -x) l2);;

(** print_poly *)
let print_poly lst =
  mapi
  (fun i x ->
    print_int x;
    if i < (List.length lst)-1 then (
      print_string " * (x ^ ";
      print_int ((List.length lst) - i - 1);
      print_string ") + "
    ) else ()
  )
  lst;;

let calc_poly ls x =
  List.fold_left
    (+.)
    0.0
    (mapi
      (fun i y -> (float_of_int y) *. (x ^.^ (List.length ls) - i - 1))
      ls
    );;

print_poly [2; 2; 8];;
print_newline ();;
print_float (solve_poly [2; 2; 8] 3.0);;


