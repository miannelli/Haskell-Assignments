-- Problem 0
-- Using Euclid's Algorithm
gcd' :: Int -> Int -> Int
gcd' x 0 = x
gcd' x y = gcd' y (mod x y)

-- Problem 1
-- Implementations of length function
-- a. Using Recursion
len1 [] = 0
len1 (x:xs) = 1 + len1(xs)
-- b. Using List Comprehension and Sum
len2 x = sum [ 1 | _ <- x ]

-- Problem 2
forall p [] = True
forall p (l:ls) = (p l) && (forall p ls)

-- Problem 3
concatenate x [] = x
concatenate [] x = x
concatenate x (y:ys) = concatenate (x : y) ys

[] 		||| ys = ys
(x:xs) 	||| ys = x : ys ||| xs

-- Problem 5
permutations' []  = [[]]
permutations' xxs = [(y:ys) | (y,xs) <- picks xxs, ys <- permutations' xs]
  where
    picks (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- picks xs]


-- Problem 6
merge xs [] = xs
merge [] xs = xs
merge (x:xs) (y:ys) = if x <= y
					then [x] ++ merge xs (y:ys)
					else [y] ++ merge (x:xs) ys

mergesort [] = []
mergesort xs = merge (mergesort (take n xs)) (mergesort (drop n xs))
  where
  	l  = (length xs)
  	n  = (div l 2)




interleave xs [] = [xs]
interleave [] xs = [xs]
interleave (x:xs) (y:ys) = 	[x:s | s <- (interleave xs (y:ys))] ++ 
							[y:s | s <- (interleave (x:xs) ys)]

