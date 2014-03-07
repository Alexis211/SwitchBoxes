
type perm = int array

type box = int * int

type boxsys = box list

type pretest = bool array array

module Perm = struct
    type t = perm
    let compare = Pervasives.compare
end

module Permset = Set.Make(Perm)


(* fact : int -> int *)
let rec fact = function
    | 0 -> 1
    | n -> n * (fact (n - 1))

let p0 n = Array.init n (fun i -> i)

let prand n =
    let p = p0 n in
    for i = 1 to n-1 do
        let j = Random.int i in
        let u, v = p.(i), p.(j) in
        p.(i) <- v;
        p.(j) <- i
    done;
    p

let copy_matrix m =
    Array.init (Array.length m) (fun i -> Array.copy m.(i))

let id_matrix n =
    let m = Array.make_matrix n n false in
    for i = 0 to n-1 do
        m.(i).(i) <- true
    done;
    m

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

(* with_new_box_pretest : int -> pretest -> box -> unit (le pretest est modifié en place) *)
let with_new_box_pretest n pretest ((i, j) : box) =
    for x = 0 to n-1 do
        if pretest.(x).(i) then
            pretest.(x).(j) <- true;
        if pretest.(x).(j) then
            pretest.(x).(i) <- true;
    done

let check_pretest n pretest =
    let ok = ref true in
    for i = 0 to n-1 do
        for j = 0 to n-1 do
            if not pretest.(i).(j) then ok := false
        done
    done;
    !ok


(* gen_boxsys : int -> boxsys -> Permset.t *)
let gen_boxsys n (bs : boxsys) =
    let rec make_permset = function
    | [] -> Permset.singleton (p0 n)
    | box::boxes ->
        with_new_box (make_permset boxes) box
    in
    make_permset bs

(* g_best_boxset : int -> int -> boxsys * Permset.t *)
(* Algorithme glouton ne fonctionnant pas *)
let rec g_best_boxset n = function
    | 0 ->
        [], Permset.singleton (p0 n)
    | k ->
        let prev_best_boxsys, prev_best_permset = g_best_boxset n (k-1) in
        let best_newperm = ref (-1, -1, prev_best_permset) in  (* on enregistre le i, le j et le permset *)
        for i = 0 to n-1 do
            for j = i+1 to n-1 do
                let new_permset = with_new_box prev_best_permset (i, j) in
                let _, _, next_permset = !best_newperm in
                if Permset.cardinal new_permset > Permset.cardinal next_permset then
                    best_newperm := (i, j, new_permset)
            done
        done;
        let i, j, new_permset = !best_newperm in
        (i, j)::prev_best_boxsys, new_permset
            
(* max_boxset : int -> int -> boxsys * Permset.t *)
let max_boxset n k =
    let target = fact n in
    let max_permset = ref ([], Permset.empty) in
    let rec aux k' boxsys permset =
        if Permset.cardinal permset > Permset.cardinal (snd !max_permset) then
            max_permset := (boxsys, permset);
        if k' > 0 then
            let pi = prand n in
            for u = 0 to n-1 do
                let i = pi.(u) in
                for v = u+1 to n-1 do
                    let j = pi.(v) in
                    let new_permset = with_new_box permset (i, j) in
                    if Permset.cardinal new_permset > Permset.cardinal permset &&
                        Permset.cardinal (snd !max_permset) < target then
                        aux (k' - 1) ((i, j)::boxsys) new_permset
                done
            done
    in
        aux k [] (Permset.singleton (p0 n));
        !max_permset
    


let () =
    let n, k = 6, 12 in
    let boxset, permset = max_boxset n k in
    List.iter (fun (i, j) -> Format.printf "(%d, %d) " i j) boxset; Format.printf "@.";
    Format.printf "Got %d permutations, %d! = %d@." (Permset.cardinal permset) n (fact n)
    



