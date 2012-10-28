(*
 * lines.ml
 * 
 * Copyright 2012 Ale110 <pooh110andco@ya.ru>
 *
 * This program is free software. It comes without any warranty, to
 * the extent permitted by applicable law. You can redistribute it
 * and/or modify it under the terms of the Do What The Fuck You Want
 * To Public License, Version 2, as published by Sam Hocevar. See
 * http://sam.zoy.org/wtfpl/COPYING for more details.
 *)

type line = NoLine | Line of float * float;;

let line_intersect p q =
  match (p, q) with
    | (NoLine, _) -> NoLine
    | (_, NoLine) -> NoLine
    | (Line (x1, y1), Line (x2, y2)) ->
      if (y1<x2) || (y2<x1) then NoLine
      else (
        (* Intersection exists - just take 2 middle points *)
        let points = List.sort
          (fun x y -> if x>y then 1 else -1)
          [x1; y1; x2; y2]
        in Line ((List.nth points 1), (List.nth points 2))
      );;

let line_union p q =
  match (p, q) with
    | (NoLine, _) -> NoLine
    | (_, NoLine) -> NoLine
    | (Line (x1, y1), Line (x2, y2)) ->
      if (y1<x2) || (y2<x1) then failwith "line_union"
      else (
        let points = List.sort
          (fun x y -> if x>y then 1 else -1)
          [x1; y1; x2; y2]
        in Line ((List.nth points 0), (List.nth points 3))
      );;

let print_line ln = match ln with
  | NoLine -> print_string "No line"
  | Line (x, y) ->
    print_string "[";
    print_float x;
    print_string ", ";
    print_float y;
    print_string "]";;
