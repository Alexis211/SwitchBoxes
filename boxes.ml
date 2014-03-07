


type box = int * int
type command = bool



(******************************************************************************)
(* get_permutation : int -> box list -> command list -> int array             *)
(* Prend un entier, une liste de boites, une liste de commandes associée, et  *)
(* calcule la permutation générée par ces listes.                             *)
let get_permutation n box_list command_list =
  try
    List.fold_left2
      (fun permutation (box_fst,box_snd) command ->
	let _ =
	  if command then
	    begin
	      let e = permutation.(box_fst) in
	      permutation.(box_fst) <- permutation.(box_snd);
	      permutation.(box_snd) <- e
	    end
	in
	permutation)
      (let permutation = Array.create n 0 in
       for i = 1 to n-1 do permutation.(i) <- i done; permutation)
      box_list
      command_list
  with
    | Invalid_argument "List.fold_left2" ->
      raise (Invalid_argument "get_permutation")



(******************************************************************************)
(* apply_permutation : int array -> bool array -> unit                        *)
(* Prend une permutation et un tableau de booléens qui représente l'état du   *)
(* circuit avant la permutation, et modifie cet array en lui appliquant la    *)
(* permutation en question.                                                   *)
let apply_permutation permutation wire_state =
  let n = Array.length permutation in
  if Array.length wire_state <> n then
    raise (Invalid_argument "apply_permutation")
  else
    begin
      let wire_state_after = Array.create n false in
      for i = 0 to n-1 do
	wire_state_after.( permutation.(i) ) <- wire_state.(i)
      done;
      for i = 0 to n-1 do
	wire_state.(i) <- wire_state_after.(i)
      done
    end



(* Plus tard, pour réduire les calculs, on peut supposer que la première      *)
(* transposition est (0,1), et que la deuxième est (0,2) ou (2,3), par        *)
(* symétries.                                                                 *)



(******************************************************************************)
(* next_bitarray : bool array -> bool                                         *)
(* Prend un array de booléens (représentant un nombre binaire) et             *)
(* l'incrémente. Renvoit false la plupart du temps, et true dès qu'il y a un  *)
(* overflow, ie qu'on repasse à O_(2).                                        *)
let next_bitarray array =
  let array_size = Array.length array in
  let rec aux_next_bitarray pos bit =
    if pos<array_size then
      begin
	let result = aux_next_bitarray (pos+1) (array.(pos)&&bit) in
	array.(pos) <- (if bit then not else (fun x -> x)) (array.(pos));
	result
      end
    else
      bit
  in
  aux_next_bitarray 0 true



(******************************************************************************)
(* enumerate_bitarrays : int -> bool array list                               *)
(* Renvoit la liste de tous les tableaux de booléens de la taille de l'entier *)
(* donné.                                                                     *)
let enumerate_bitarrays n =
  let b = Array.make n false in
  let result = ref [Array.copy b] in
  while not(next_bitarray b) do
    result := (Array.copy b) :: !result
  done;
  !result



(******************************************************************************)
(* test_boxlist: int -> box list -> int                                       *)
(* Prend une taille de nappe de fils, une liste de boites et renvoit le       *)
(* nombre de permutations qu'elle engendre.                                   *)
let test_boxlist n box_list =
  let rec count_nodoubles = function
    | [] -> 0
    | t::(t2::_ as q) when t=t2 -> count_nodoubles q
    | t::q -> 1+(count_nodoubles q)
  in
  (count_nodoubles
    (List.fast_sort
       Pervasives.compare
       (List.map
	  (fun a -> get_permutation n box_list (Array.to_list a))
	  (enumerate_bitarrays n))))



(******************************************************************************)
(* fact : int -> int                                                          *)
let rec fact = function
  | 0 -> 1
  | n -> n * (fact (n-1))



(******************************************************************************)
(* calc_boxes : int -> box list                                               *)
(* Calcule l'ensemble des boites possibles (i,j) qui ne sont pas triviales    *)
(* (i=j) et qui sont telles que i<j.                                          *)
let calc_boxes n =
  let rec aux_calc_boxes = function
    | [] -> if n=1 then [] else aux_calc_boxes [0,1]
    | (i,j)::q as l ->
      if j<n-1 then aux_calc_boxes ((i,j+1)::l)
      else
	begin
	  if i<n-2 then aux_calc_boxes ((i+1,i+2)::l)
	  else l
	end
  in
  aux_calc_boxes []



(******************************************************************************)
(* calc_boxlists : int -> int -> box list list                                *)
(* [calc_boxlists n k] renvoit la liste de toutes les combinaisons de taille  *)
(* k de boites sur n fils.                                                    *)
let calc_boxlists n k =
  let boxes = calc_boxes n in
  let rec aux_calc_boxlists l boxlists =
    if l >= k then boxlists
    else
      aux_calc_boxlists (l+1)
	(List.concat
	   (List.map
	      (fun boxlist -> List.map (fun box -> box::boxlist) boxes)
	      boxlists))
  in
  if n>=4 then
    List.map
      (List.rev)
      (aux_calc_boxlists 2 [[2,3;0,1];[1,2;0,1]])
  else if n>=3 then
    List.map
      (List.rev)
      (aux_calc_boxlists 2 [[1,2;0,1]])
  else if n>=2 then
    List.map
      (List.rev)
      (aux_calc_boxlists 1 [[0,1]])
  else
    [[]]



(* Test *)
let test n k =
  let fn = fact n in
  let rec aux_test = function
    | [] -> []
    | boxlist::q ->
      let p = (test_boxlist k boxlist) in
      if p = fn then (boxlist,p)::(aux_test q)
      else aux_test q
  in
  aux_test (calc_boxlists n k)



let launcher n =
  let k = ref n in
  while (List.length (test n (!k))) = 0 do
    Format.printf "Rien trouvé pour n=%d, k=%d.@." n (!k);
    incr k
  done;
  Format.printf "Résultat obtenu pour n=%d, k=%d.@." n (!k)
  
let _ =
  Arg.parse
    []
    (fun s -> launcher (int_of_string s))
    ":)"





(* TABLEAUX *)
