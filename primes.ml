(* primes.ml
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

let next_prime lst = 
  let rec next_prime_inner n ls =
    if (List.for_all (fun x -> (n mod x != 0)) ls) then
      n
    else
      next_prime_inner (n+1) ls
  in next_prime_inner (List.nth lst ((List.length lst)-1)) lst;;

(* TODO: делает не до числа n, а n чисел - ? *)
let prime_list n = 
  let rec prime_list_inner i lst =
    if i>n then
      lst
    else (
      prime_list_inner (i+1) (lst @ [next_prime lst])
    )
  in prime_list_inner 11 [2; 3; 5; 7];;
  
print_list (print_int) (prime_list 999);;
