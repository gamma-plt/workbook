let pow b n = if n = 1 then b else b * (pow b (n - 1)) in 
	pow 2 3
end