(* WORKING! *)

let even n = (n % 2) = 0 in
	let sumeven xs acc = 
		match xs with
			[] -> acc
			| h::t -> 
				if even h then sumeven t (acc + h)
				else sumeven t acc
	in sumeven [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 0 end
end