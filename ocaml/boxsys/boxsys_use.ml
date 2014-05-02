

type boxsys      = Boxsys_ast.boxsys
type command     = Boxsys_ast.command
type permutation = Boxsys_ast.permutation


let get_permutation ((nb_wires, box_list):boxsys) (command:command) :permutation =
  let swap t (i,j) = let e = t.(i) in t.(i) <- t.(j); t.(j) <- e in
  List.fold_left2
    (fun arr box com -> if comm then swap arr box; arr)
    (Array.init n (fun i -> i))
    box_list
    command


let get_CCD_2pow k : boxsys =
  let nb_wires = 1 lsl k in
  let boxsys = ref [] in
  for log_group_size = 1 to 2*k - 1 do
    let log_group_size =
      if log_group_size > k then
	2*k-log_group_size
      else
	log_group_size
    in
    let group_size = 1 lsl log_group_size in
    for nb_box = 1 to n/group_size do
      for nb_wire = 1 to group_size/2 do
	let wire = (nb_box-1)*group_size + nb_wire - 1 in
	boxsys := (wire, wire+group_size/2) :: !bs
      done
    done;
    List.rev !bs

let get_CCD (nb_wires:int) :boxsys =
  let k = int_of_float (ceil (log (float_of_int n)/.log 2.)) in
  List.filter (fun (i,j) -> i<n && j<n) (get_CDD_2pow k)
