open Printf

type grid = int list list

(* question 3.1 *)
let g_example = [[-2;0;1;4];[7;2;-3;-4];[6;-1;3;5]]



(* question 3.2 
height : grid -> int qui, pour une grille g donnée en
argument, renvoie la longueur de la première liste de g, ou une exception si g est vide   
*)
let height = function
| [] -> failwith "grille est vide" 
| l::g -> (List.length l)



(* question 3.3 
wf_grid_exn : grid -> unit qui renvoie une exception
si et seulement si l’argument donné n’est pas un grille bien formée.
*)
exception Mal_forme of string
let rec wf_grid_exn g = 
  let height_g = height g in 
  if (height_g != 0 )then (check_wf height_g g ) else ()
and check_wf height = function
| [] -> () 
| l::g -> if ((List.length l)!=height) then raise(Mal_forme "mal formee") else 
          (check_wf height g)



(* question 3.4 
  4+7+6=17 
  car c'est la max de chaque column et se trouver a [i-1;i+1] a partir de la case d'avant, donc c'est un chemin valide de taille de 17   
*)



(* question 3.5
rotate_up : 'a list -> 'a list qui, appliquée à une
liste non-vide [i0; i1; ...; in] donne la liste [i1; ...; in; i0].   
*)

let rotate_up = function 
| [] -> failwith " liste vide "
| a::l -> (l@[a])



(* question 3.6 
rotate_down : 'a list -> 'a list qui, appliquée à
une liste non-vide [i0; i1; ...; in] donne la liste [in; i0; i1; ...; in−1]
*)
let rotate_down l = 
  let rec rotate_down_bis r = function
  | [] -> failwith "liste vide"
  | a::[] -> a::r 
  | a::l -> rotate_down_bis (r@[a]) l
in rotate_down_bis [] l 




(* question 3.7 *)
let rec best_option l = 
  let ld = rotate_down l in match ld with 
  | [] | [_] -> ld 
  | a::b::[] -> let maxall = max a b in [maxall;maxall]
  | a::b::c::_ -> best_option_bis a b ld
and best_option_bis hd hdd= function
| [] -> [] 
| [a] -> [(max (max a hd) hdd )]
| a::b::[] -> ((max (max a b) hd )::(best_option_bis hd hdd ([b])) )
| a::b::c::ldd -> ((max (max a b) c )::(best_option_bis hd hdd (b::c::ldd)) )
and max a b = if a<b then b else a



(*  question 3.8 *)
let rec sums = function
| [] -> failwith "grille vide"
| c::g -> (sums_bis c g)
and sums_bis bo = function
| [] -> bo
| c::g -> (sums_bis ( add (best_option bo)  c) g)
and add bo c = match (bo,c) with 
| _, [] | [], _ -> failwith "list de taille differente"
| a::[], b::[] -> [(a+b)]
| a::boo, b::cc -> ((a+b)::(add boo cc))



(* question 3.9*)
let max_list = function
| [] -> failwith "list vide"
| a::l -> List.fold_left (fun max e -> if max<e then e else max) a l


(* question 3.10 *)
let solve g = wf_grid_exn g; (max_list (sums g))

let x = 0 
(* tests*)
let print_list l = List.iter (fun a -> printf "%d " a) l; printf "\n"
let a = [8;4;5;6;7]
let () = 
  printf "height = %d \n" (height g_example);;
  try (wf_grid_exn g_example) with Mal_forme(s) -> printf "%s\n" s;;
  printf "list a\n";;
  print_list a;;
  printf "rotate up\n";;
  print_list (rotate_up a);;
  printf "rotate down \n";;
  print_list (rotate_down a);;
  printf "best option\n";;
  print_list a;;
  print_list (best_option a);;
  print_list ( sums g_example);;
  printf "max a = %d\n" (max_list a);;
  printf "solve g_exemple = %d\n" (solve g_example);;
  
  