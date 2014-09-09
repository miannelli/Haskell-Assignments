-- Problem 0. (trivial) Implement the gcd function recursively.
-- Using Euclid's Algorithm
gcd' :: Int -> Int -> Int
gcd' x 0 = x
gcd' x y = gcd' y (mod x y)


-- Problem 1. (trivial) Write two simple implementations of the builtin length function:
-- a. using recursion (and pattern matching)
len1 [] = 0
len1 (x:xs) = 1 + len1(xs)
-- b. using list comprehension and sum
len2 x = sum [ 1 | _ <- x ]


-- Problem 2. (trivial) Implement a forall function which checks if a property holds for 
-- each element of a list. It takes a predicate p (a one-argument function 
-- returning a boolean) and a list l, and returns a Bool:
forall p [] = True
forall p (l:ls) = (p l) && (forall p ls)


-- Problem 3. (easy) Write a function app that is equivalent to the built-in list 
-- concatenation function ++. 
concatenate x [] = x
concatenate [] x = x
concatenate (x:xs) ys = x : (concatenate xs ys)
-- a. make sure your implementation runs in linear-time
-- Prepending an element into a list is O(1) time, and concatenate
-- prepends a number of times equal to (x:xs) :. it is linear time
-- b. what's wrong with the following implementation? 
-- what's the time complexity?
-- app a [] = a
-- app a (x:xs) = app (a++[x]) xs
-- The first issue is that it uses the ++ function, meaning that it answers the question
-- using a function we are assuming we have to implement.
-- The next issue is that appending is linear time complexity, since it must prepend the
-- entire first list the second.  A new list is being created a number of times equal to the
-- length of (x:xs), and we are prepending each of the elements of a to each of these lists
-- making the time complexity quadratic.


-- Problem 4. (easy) An interleaving of two lists contains all elements from both lists, 
-- with the constraint that any two items from the same list are still in the same order.
interleave xs [] = [xs]
interleave [] xs = [xs]
interleave (x:xs) (y:ys) = 	[x:s | s <- (interleave xs (y:ys))] ++ 
							[y:s | s <- (interleave (x:xs) ys)]

-- Problem 5. (medium) Write a function that takes a list as argument and returns a list
-- containing all permutations of the input list
permutations' []  = [[]]
permutations' xxs = [(y:ys) | (y,xs) <- picks xxs, ys <- permutations' xs]
  where
    picks (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- picks xs]


-- Problem 6. (easy-medium) Implement mergesort.
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

-- What is the (average case and worst case) complexity of your code? 


-- Problem 7. (easy) Implement quickselect: return the kth smallest element of a list.
-- (see CLRS, Section 9.2, for details if you're not familiar with this)

-- What is the (average case and worst case) complexity of your code?  

-- 8. (in your own words) Compared to imperative programming, what aspects of 
-- functional programming do you like the most, and what do you dislike the most?

--Debriefing (required!): --------------------------

-- 1. Approximately how many hours did you spend on this assignment?

-- 2. Would you rate it as easy, moderate, or difficult?
-- A. 	I would say it is moderate difficulty for a beginner.  Nothing really felt out
--		too far out of reach and it was nothing that couldn't be solved without spending 
--		some time and doing research.
-- 3. Did you work on it mostly alone, or mostly with other people?
--		Alone.
-- 4. How deeply do you feel you understand the material it covers (0%â€“100%)? 
-- 5. Any other comments?

--This section is intended to help us calibrate the homework assignments. 
--Your answers to this section will *not* affect your grade; however, skipping it
--will certainly do.



