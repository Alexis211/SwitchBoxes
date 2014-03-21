type perm = int array
type box = int * int
type boxsys = box list

module Perm = struct
  type t = perm
  let compare = Pervasives.compare
end
module Permset = Set.Make(Perm)


(* On récupère le testeur de boites de boxes2.ml *)

(* with_new_box : Permset.t -> box -> Permset.t *)
let with_new_box (p : Permset.t) ((i, j) : box) =
    Permset.union p
        (Permset.fold
            (fun perm pset ->
                let perm2 = Array.copy perm in
                perm2.(i) <- perm.(j);
                perm2.(j) <- perm.(i);
                Permset.add perm2 pset)
            p
            Permset.empty)

let rec fact = function
  | 0 -> 1
  | n -> n*fact(n-1)

let test_boxsys n (bs : boxsys) =
  Permset.cardinal
    (List.fold_left
       with_new_box
       (Permset.singleton (Array.init n (fun i -> i)))
       bs)
  == fact n

let n = 3
let bs = [0,1;1,2;0,1]

let n = 4
let bs = [ 0,1; 2,3; 0,2; 1,3; 0,1; 2,3 ]
let b = test_boxsys n bs in b

let n = 6
let bs = [ 0,1; 1,2; 0,1; 3,4; 4,5; 3,4; 0,3; 1,4; 2,5; 0,1; 1,2; 0,1; 3,4; 4,5; 3,4 ]
let b = test_boxsys n bs in b

let n = 8
let bs = [ 0,1; 2,3; 4,5; 6,7;
	   0,2; 1,3; 4,6; 5,7; 
	   0,4; 1,5; 2,6; 3,7;
	   0,2; 1,3; 4,6; 5,7; 
	   0,1; 2,3; 4,5; 6,7; ]
let b = test_boxsys n bs in b

(* Ne marche que pour les puissances de 2 pour le moment *)
let generate_boxsys k =
  let pow2 k = 1 lsl k in
  let n = pow2 k in  
  let bs = ref [] in
  for ltdg (* log taille des groupes *) = 1 to 2*k-1 do
    let ltdg = if ltdg > k then 2*k-ltdg else ltdg in
    let tdg = pow2 ltdg in
    for bn = 1 to n/tdg do (* : box number *)
      for fn = 1 to tdg/2 do (* : fil number *)
	let fil = (bn-1)*tdg + fn -1 in
	bs := (fil,fil+tdg/2) :: !bs;
      done
    done
  done;
  List.rev (!bs)

let k = 4 in
let b = test_boxsys (1 lsl k) (generate_boxsys k) in b
