
(* question 2.1*)

(*
   f1: 'a'*'a' -> bool -> ('a' -> bool)
   (x,y): 'a' * 'a' 
   z: bool 
   g: 'a' -> 'a' -> bool
   a: 'a'
   b: 'a'


  car ca renvoi une fonction qui attend un argument de type 'a et renvoi bool 


*)

let f1 (x , y ) z =
  let g a b = a < b in
  if z then g x else g y ;;



(*question 2.2*)
(*  %%%ocamlpro 
  list_sum : prend une liste et renvoi le nombre d'element qui respect la contrainte  p 
  list_or : prend une liste de bool et renvoi true si la liste a au moins un element true 
  - leur type: 
    list_sum : (a->bool) -> (a list -> int )
    list_or : bool list -> bool 

  // ligne 4-6: 
    - de la ligne 5 est mal type car list_sum attend en premier argument un argument de type (a'->bool)

    - le type de a : 
      4: 'a list 
      5: 'a list 
      6: bool list 

  // ligne : 8-10 
    - le meme problem car (!a) a le type 'a list, mais list_sum attend en premier argument un arg du type ('a list -> bool )

    - le type de a : 
      8: ref 'a list  
      9: ref 'a list 
      10: ref bool list 

1 let list_sum p = List . fold_left ( fun x y -> if p y then x + 1
else x ) 0;;
2 let list_or = List . fold_left ( fun x y -> x || y ) false ;;
3
4 let a = [];;
5 list_sum a ;;
6 list_or a ;;
7
8 let a = ref [];;
9 list_sum ! a ;;
10 list_or ! a ;;



*)

 


(* exercice 2.3*)

type 'a arbre_binaire =
  Feuille
  | Noeud of 'a * 'a arbre_binaire * 'a arbre_binaire ;;

(* si on veut prendre l'arbre comme premier parametre on utilise vette version 
let rec map a f = match a with 
| Feuille -> Feuille
| Noeud(a,g,d) -> Noeud((f a), (map f g), (map f d))
*)

(*('a -> b) -> 'a arbre_binaire -> b arbre_binaire *)
let rec map f = function 
| Feuille -> Feuille
| Noeud(a,g,d) -> Noeud((f a), (map f g), (map f d))


(*    ('a -> bool) -> 'a arbre_bnaire -> bool )*)
let rec forall_arbre p = function 
| Feuille -> false 
| Noeud(a,g,d) -> (p a)&&(forall_arbre p g)&&(forall_arbre p d)

