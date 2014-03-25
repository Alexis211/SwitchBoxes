{}

rule read = parse
  | '#' [^'\n']* '\n'                { read lexbuf }
  | '0'|(['1'-'9']['0'-'9']*) as i   { int_of_string i }
  | _                                { read lexbuf }
  | eof                              { -1 }

{
  
type box    = Boxsys_ast.box
type boxsys = Boxsys_ast.boxsys

let file_to_boxsys (filename:string) :boxsys =
  let in_channel = open_in filename in
  let lexbuf = Lexing.from_channel in_channel in
  let nb_wires = read lexbuf in
  let nb_boxes = read lexbuf in
  let box_list = ref [] in
  let while_condition = ref true in
  while !while_condition do
    let i = read lexbuf in
    if i = -1 then
      if List.length !box_list < nb_boxes then
	raise (Invalid_argument "Expected more boxes.")
      else
	while_condition := false
    else if i >= nb_wires then
      raise (Invalid_argument "Found box with a too high left side.") 
    else
      begin
	let j = read lexbuf in
	if j = -1 then
	  raise (Invalid_argument "Odd number of boxes.")
	else if j = i then
	  raise (Invalid_argument "Found box that concerns two times the same wire.")
	else if j >= nb_wires then
	  raise (Invalid_argument "Found box with a too high right side.") 
	else
	  begin
	    box_list := (i,j) :: !box_list;
	    if List.length !box_list > nb_boxes then
	      raise (Invalid_argument "Found too much boxes.")
	  end
      end
  done;
  close_in in_channel;
  nb_wires, List.rev !box_list

let boxsys_to_buffer (boxsys:boxsys) :Buffer.t =
  let buffer = Buffer.create 16 in
  let formatter = Format.formatter_of_buffer buffer in
  (* Je ne veux pas utiliser le str_formatter, car il pourrait être utilisé dans la
     fonction qui appelle ce module. *)
  let nb_wires, box_list = boxsys in
  let nb_boxes = List.length box_list in
  Format.fprintf formatter "%d %d\n" nb_wires nb_boxes;
  let rec aux = function
    | []       -> Format.fprintf formatter "@?"
    | (i,j)::t -> Format.fprintf formatter "%d %d\n" i j; aux t
  in
  aux box_list;
  buffer

let boxsys_to_string (boxsys:boxsys) :string =
  Buffer.contents (boxsys_to_buffer boxsys)
    
let boxsys_to_std (boxsys:boxsys) :unit =
  Buffer.output_buffer (Pervasives.stdout) (boxsys_to_buffer boxsys)

let boxsys_to_err (boxsys:boxsys) :unit =
  Buffer.output_buffer (Pervasives.stderr) (boxsys_to_buffer boxsys)

let boxsys_to_file (filename:string) (boxsys:boxsys) =
  let out_channel = open_out filename in
  Buffer.output_buffer out_channel (boxsys_to_buffer boxsys);
  close_out out_channel

}
