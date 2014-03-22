type perm = int array
type box = int * int
type boxsys = box list

module Perm = struct
  type t = perm
  let compare = Pervasives.compare
end
module Permset = Set.Make(Perm)

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

let true_gen_bs n =
  let k1 = int_of_float (ceil (log (float_of_int n)/.log 2.)) in
  List.filter (fun (i,j) -> i<n && j<n) (generate_boxsys k1)

let print_bs bs =
  let rec aux = function
    | [] -> ()
    | (i,j)::t ->
      Format.printf "(%d,%d); " i j;
      aux t
  in
  Format.printf "[ ";
  aux bs;
  Format.printf "];@."

(*
let _ =
  let k = 4 in
  let n = 1 lsl k in
  Format.printf "Début du test.\nk = %d, n = %d.@." k n;
  let bs = generate_boxsys k in
  print_bs bs;
  if test_boxsys n bs then
    Format.printf "Ca marche !@."
  else
    Format.printf "Ca ne marche pas :'(@."
*)

let _ =
  let n = ref 2 in
  while true do
    Format.printf "\nDébut du test pour n = %d.\nVoici le système de boites:@." !n;
    let bs = true_gen_bs !n in
    print_bs bs;
    Format.printf "C'est un système de %d boites.@." (List.length bs);
    if test_boxsys !n bs then
      begin
	Format.printf "Il engendre toutes les permutations.@.";
      end
    else
      begin
	Format.printf "Echec: il n'engendre pas toutes les permutations.@.";
      end;
    incr n
  done
