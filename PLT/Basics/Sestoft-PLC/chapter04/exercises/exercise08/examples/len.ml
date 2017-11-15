(* WORKING! *)

let len xs = 
	match xs with
		[] -> 0
		| h::t -> 1 + len t
	in len [1, 2, 3, 4, 5, 6]
end