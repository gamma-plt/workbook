let tuple = (1, 2, 2 + 3, let succ n = if n > 1 then n + 1 else 0 in succ 4 end, 5) in
	(#1(tuple), #2(tuple))
end