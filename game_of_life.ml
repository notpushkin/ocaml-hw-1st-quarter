(* from Ocaml 4.00.1 *)
(* val mapi : (int -> 'a -> 'b) -> 'a list -> 'b *)
let rec mapi i f = function
    [] -> []
  | a::l -> let r = f i a in r :: mapi (i + 1) f l;;

let mapi f l = mapi 0 f l;;
let iteri f l = mapi f l;;
(* /from Ocaml *)

let nth2 i j l =
  List.nth (List.nth l i) j;;

let combine2 a b =
  mapi (fun i l -> List.combine l (List.nth b i)) a;;

let life_calc_neighbours field = 
  let xl = (List.length field) - 1
  and yl = (List.length (List.nth field 0)) - 1
  in 
  mapi (fun x lst ->
    mapi (fun y el ->
      List.fold_left (+) 0 (
        List.map (fun k -> if k then 1 else 0) [
          if (x!=0  && y!=0)  then (nth2 (x-1) (y-1) field) else false;
          if (x!=0)           then (nth2 (x-1) (y)   field) else false;
          if (x!=0  && y!=yl) then (nth2 (x-1) (y+1) field) else false;
          if          (y!=0)  then (nth2 (x)   (y-1) field) else false;
          (* this element doesn't count *)
          if          (y!=yl) then (nth2 (x)   (y+1) field) else false;
          if (x!=xl && y!=0)  then (nth2 (x+1) (y-1) field) else false;
          if (x!=xl)          then (nth2 (x+1) (y)  field) else false;
          if (x!=xl && y!=yl) then (nth2 (x+1) (y+1) field) else false
        ]
      )
    ) lst
  ) field;;

let life_turn field =
  List.map (fun lst ->
    List.map (fun (el, n) ->
      if (el = true) then (
        if (n = 3) then true
        else false
      ) else (
        if (n = 2) || (n = 3) then true
        else false
      )
    ) lst
  ) (combine2 field (life_calc_neighbours field));;
    
let life_print field =
  List.iter (fun l ->
    List.iter (fun el ->
      if el then print_string "X"
      else print_string "."
    ) l;
    print_newline()
  ) field;;

(*
let t = true;;
let f = false;;

let thefield = [
  [t; t; f; t; f; t; f; t; f; t];
  [f; f; t; f; t; f; t; f; t; f];
  [t; t; f; t; f; t; f; t; f; t];
  [f; f; t; f; t; f; t; f; t; f];
  [t; t; f; t; f; t; f; t; f; t];
  [f; f; t; f; t; f; t; f; t; f];
  [t; t; f; t; f; t; f; t; f; t];
  [f; f; t; f; t; f; t; f; t; f];
  [t; t; f; t; f; t; f; t; f; t];
  [f; f; t; f; t; f; t; f; t; f]
];;

life_print thefield;;
print_newline();;
life_print (life_turn thefield);;
*)









