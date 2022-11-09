exception Out_of_terrain of string
open Printf


(* question 5.1 *)
type commandv1 = 
  | Up 
  | Down  
  | Left  
  | Right 
  | Seq of commandv1 list 


let rec evalposv1 = function
  | Up -> (0, 1)
  | Down -> (0, -1)
  | Left -> (-1, 0)
  | Right -> (1, 0)
  | Seq(l) -> List.fold_left (fun pos c ->( let x,y = (addcpl (pos,(evalposv1 c))) in in_terrain x y ) ) (0,0) l
  
and addcpl = function
  | (a,b),(c,d) -> (a+c,b+d)
and in_terrain x y =
  if ((x>100) || (x<(-100)) || y>100 || y<(-100)) then 
    (raise (Out_of_terrain "en dehors du terrain"))
  else (x,y) 

(* tests*)
let c1 = Seq([Up;Up;Right;Seq([Down;Left;Up;Left])])
let () =
  let x,y = evalposv1 c1 in 
  printf "posv1 = (%d,%d)\n" x y ;;
  


(* version2 : question 5.2*)
 
type commandv2 = 
  | Up 
  | Down  
  | Left  
  | Right 
  | Seq of commandv2 list 
  | For of commandv2 * int 


let rec evalposv2 = function
  | Up -> (0, 1)
  | Down -> (0, -1)
  | Left -> (-1, 0)
  | Right -> (1, 0)
  | Seq(l) -> List.fold_left (fun pos c ->( let x,y = (addcpl (pos,(evalposv2 c))) in in_terrain x y ) ) (0,0) l
  | For(c,n) -> (evalpos_for n c) 

and evalpos_for n = function
  | Up ->  in_terrain 0 n 
  | Down -> in_terrain 0 (-n)
  | Left -> in_terrain (-n) 0
  | Right -> in_terrain  n 0
  | Seq(l) -> List.fold_left (fun acc ec -> let x,y = (addcpl (acc,(evalpos_for  n ec))) in in_terrain x y ) (0,0) l
  | For(c,m) -> evalpos_for m c 

and addcpl = function
  | (a,b),(c,d) -> (a+c,b+d)

and in_terrain x y =
  if ((x>100) || (x<(-100)) || y>100 || y<(-100)) then 
    (raise (Out_of_terrain "en dehors du terrain"))
  else (x,y) 

 (* test v2*)
let c2 = Seq([For(Up,5);Up;For(Up,5);Seq([Down;Left;Up;For(Left,4)])])
let () =
  let x,y = evalposv2 c2 in 
  printf "posv2 = (%d,%d)\n" x y ;;





  
(* version final v3 ( question 5.3) *)
type command = 
  | Up 
  | Down  
  | Left  
  | Right 
  | Seq of command list 
  | For of command * int 
  | While_true of command

let rec evalpos = function
  | Up -> (0, 1)
  | Down -> (0, -1)
  | Left -> (-1, 0)
  | Right -> (1, 0)
  | Seq(l) -> List.fold_left (fun pos c ->( let x,y = (addcpl (pos,(evalpos c))) in in_terrain x y ) ) (0,0) l
  | For(c,n) -> (evalpos_for n c) 
  | While_true(c) -> (0,0) (* just pour ne pas avoir le warning *)


  and evalpos_for n = function
    | Up ->  in_terrain 0 n 
    | Down -> in_terrain 0 (-n)
    | Left -> in_terrain (-n) 0
    | Right -> in_terrain  n 0
    | Seq(l) -> List.fold_left (fun acc ec -> let x,y = (addcpl (acc,(evalpos_for  n ec))) in in_terrain x y ) (0,0) l
    | For(c,m) -> evalpos_for m c 
    | While_true(c) -> (0,0) (* just pour ne pas afficher le warning*)

  and addcpl = function
    | (a,b),(c,d) -> (a+c,b+d)
  and in_terrain x y =
    if ((x>100) || (x<(-100)) || y>100 || y<(-100)) then 
      (raise (Out_of_terrain "en dehors du terrain"))
    else (x,y) 




let safety c = 
  try
    match  evalpos c with
    | (0,0) -> true
    | _ -> false
  with 
    | Out_of_terrain(_) -> false


(* test *)
let ct = Seq([For(Up,5);For(Down,10);For(Up,5);Seq([Down;Left;Up;Right])])
let cf = Seq([For(Up,5);For(Down,8);For(Up,5);Seq([Down;Left;Up;Right])])

let () =
  printf "is_safe(c_true) = %b\n" (safety ct) ;;
  printf "is_safe(c_false) = %b\n" (safety cf) ;;
  