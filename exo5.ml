

type command = Up | Down | Left | Right | Seq of command list


(* question  5.1 *)
let evalpos c = 
  let evalpos_bis pos = function
  | Up -> (fst pos, 1+(snd pos))
  | Down -> (fst pos, -1+(snd pos))
  | Left -> (-1+(fst pos), snd pos)
  | Right -> (1+(fst pos), snd pos)
  | Seq(l) -> ( match l with 
    | [] -> pos
    | c::l -> let pos_c = evalpos_bis pos c in evalpos_bis pos_c l)  
in evalpos_bis (0,0) c 
