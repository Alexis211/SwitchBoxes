

(* calcul de f(n) = log_2(n!) *)
let rec log_fact n =
	let acc = ref 0. in
	for i = 1 to n do
		acc := !acc +. log(float_of_int i)
	done;
	!acc

let f n = log_fact n /. log 2.

(* calcul de g(n) = n*(log_2(n) - 1/2) *)
let g n =
	let n = float_of_int n in
	n *. (log n /. log 2. -. 0.5)

(* calcul de a(n) = sum_k=1^n ceil(log_2(k)) *)
let a n =
	let acc = ref 0. in
	for i = 1 to n do
		acc := !acc +. ceil (log (float_of_int i) /. log 2.)
	done;
	!acc


let () =
	for i = 1 to 30 do
		let showh x =
			let gg = g x in
			let aa = a x in
			let ff = ceil (f x) in
			Format.printf "%8d  \t %.2f   \t/ %.2f  \t= %f@." x gg aa (gg /. aa)
		in showh (1 lsl i); showh ((1 lsl i) lor (1 lsl (i-1)))
	done


(* conclusion : g(n) ~ h(n) en l'infini *)
