let notDivisible d n = n % d <> 0 in
	let test a b c =
		if a <= b then (notDivisible a c) && (test (a + 1) b c)
		else notDivisible b c in
			let prime n = (n = 2) || (test 2 (n - 1) n) in
				prime 11
			end
	end
end