open Printf 

let add a b = a+b 



(* question 1.1*)

let f a b = a + b
let ajout_deux g = if g = 0 then f 2 0 else (f (2+ 1) (g) - 1)
let ajout_deux_bis g = if g = 0 then f 2 0 else f (2+ 1) (g - 1)


(*question 1.2*)
(*
1 -> rec 
2 -> @ -> ::
3 -> (match ...)
4 -> (match ...) %%%some other bug 
*)
let rec somme_derniers l1 l2 = match l1 with
  | [] -> (match l2 with
    | [] -> 0
    | x2 :: [] -> x2
    | hd2 :: tl2 -> somme_derniers [] tl2)
  | x1 :: [] -> (match l2 with
    | [] -> x1
    | x2 :: [] -> x1 + x2
    | hd2 :: tl2 -> somme_derniers [ x1 ] tl2)
  | hd1 :: tl1 -> ( somme_derniers tl1 l2)



(* exercice 1.3*)
(*
   1 => IN 
   2 => BEGIN 
   3 => CHANGER SOIR TOUT A FLOAT OU TOUT A INT 

*)



let x a b = a +. b
let f y z = 
  let v = y +. 5. in 
  if (z > y) then begin 
    printf "%f\n" z ; 
    z
  end 
  else (x v 0.)

(* et si on considere que x prend des int, sinon code ci-dessus *)
(* let x a b = a + b
let f y z = 
  let v = y + 5 in 
  if (z > y) then begin 
    printf "%d\n" z ; 
    z
  end 
  else (x v 0) *)




(* question 1.4*)


(*
   
1 let y = 5.3 ;;               
2 let u z =            (*u:2 z:2*)       
3 z -. y ;;            (*z:2 y:1*)   
4 let y =              (*y:4 *)  
5 let y =              (*y:5 *)    
6 y +. 2.5             (*y:5 *)   
7 in let z = u y       (*u:2 z:7 y:5*)        
8 in y *. z            (*z:7 y:5*)       
9 in let u = 2.0 in    (*u:9*)           
10 y +. y +. u ;;      (*u:9 y:5*)         


*)








(* test*)


let () = 
  printf "%d\n" (ajout_deux 4);;
  printf "%d\n" (ajout_deux_bis 4);;
  printf "%d\n" (somme_derniers [3;5] [5;-1])