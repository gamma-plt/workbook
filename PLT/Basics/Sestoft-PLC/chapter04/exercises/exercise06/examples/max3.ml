let max a b = if a > b then a else b in
	let max3 a b c = max (max a b) c in
		max3 34 67 23
	end
end