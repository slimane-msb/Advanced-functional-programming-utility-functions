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
let rec wf_grid_exn g = 
  let height_g = height g in 
  check_wf height_g g 
and check_wf height = function
| [] -> () 
| l::g -> if ((List.length l)==height) then failwith "mal forme" else 
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
let rec rotate_down = function
| [] -> failwith "liste vide "
| a::[] -> [a]
| a::b::l -> b::a::(rotate_down l)




(* question 3.7 *)
let rec best_option l = 
  let ld = rotate_down l in 
  best_option_bis ld 
and best_option_bis = function
| [] -> [] 
| [a] -> [a]
| a::b::[] -> let maxall = max a b in [maxall;maxall]
| a::b::c::ldd -> ((max (max a b) c )::(best_option_bis (b::c::ldd)) )
and max a b = if a<b then a else b 


(*  question 3.8 %%% change avec c not sure a cause de c das 3.7 *)
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


(* tests*)

(* les test a la fin *)