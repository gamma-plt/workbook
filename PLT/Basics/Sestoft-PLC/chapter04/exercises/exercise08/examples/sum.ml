(* WORKING! *)

let sum xs = 
	match xs with
		[] -> 0
		| h::t -> h + sum t
	in sum [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
end