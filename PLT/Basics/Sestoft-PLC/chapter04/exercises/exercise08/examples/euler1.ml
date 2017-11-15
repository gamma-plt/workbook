(* If we list all the natural numbers below 10 that are multiples of 3 or 5, 
we get 3, 5, 6 and 9. The sum of these multiples is 23. *)

let multiple n = (n % 3 = 0) || (n % 5 = 0) in
	let sum xs =
		match xs with
			[] -> 0
			| x::xs -> 
				if multiple x then x + (sum xs)
				else sum xs
	in sum [1, 2, 3, 4, 5, 6, 7, 8, 9] end
end