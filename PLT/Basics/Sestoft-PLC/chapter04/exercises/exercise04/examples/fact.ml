let fact n = 
	let factAux n acc =
		if n < 2 then acc
		else factAux (n - 1) (n * acc)
	in
		factAux n 1 end
in fact 6 end