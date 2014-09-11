-- Michael Iannelli
-- CSC71010
-- Homework Assignment #01

-- Problem 0. (trivial) Implement the gcd function recursively.
-- Using Euclid's Algorithm
gcd' :: Int -> Int -> Int
gcd' x 0 = x
gcd' x y = gcd' y (mod x y)


-- Problem 1. (trivial) Write two simple implementations of the builtin length 
-- function:
-- a. using recursion (and pattern matching)
len1 :: [a] -> Int
len1 [] 	= 0
len1 (x:xs) = 1 + len1(xs)
-- b. using list comprehension and sum
len2 :: [a] -> Int
len2 x = sum [ 1 | _ <- x ]


-- Problem 2. (trivial) Implement a forall function which checks if a property 
-- holds for each element of a list. It takes a predicate p (a one-argument  
-- function returning a boolean) and a list l, and returns a Bool:
forall :: (a -> Bool) -> [a] -> Bool
forall p [] 	= error "List can not be empty"
forall p [x] 	= p x
forall p (l:ls) = (p l) && (forall p ls)


-- Problem 3. (easy) Write a function app that is equivalent to the built-in  
-- list concatenation function ++. 
concatenate :: [a] -> [a] -> [a]
concatenate x [] 		= x
concatenate [] x 		= x
concatenate (x:xs) ys 	= x : (concatenate xs ys)
-- a. make sure your implementation runs in linear-time
-- Prepending an element into a list is O(1) time, and concatenate
-- prepends a number of times equal to (x:xs) :. it is linear time
-- b. what's wrong with the following implementation? 
-- what's the time complexity?
-- app a [] = a
-- app a (x:xs) = app (a++[x]) xs
-- The first issue is that it uses the ++ function, meaning that it answers the 
-- question using a function we are assuming we have to implement.
-- The next issue is that appending is linear time complexity, since it must 
-- prepend the entire first list the second.  A new list is being created a 
-- number of times equal to the length of (x:xs), and we are prepending each of 
-- the elements of a to each of these lists making the time complexity
-- quadratic (O(a^2)).		


-- Problem 4. (easy) An interleaving of two lists contains all elements from  
-- both lists, with the constraint that any two items from the same list are 
-- still in the same order.
interleave :: [a] -> [a] -> [[a]]
interleave xs [] 		 = 	[xs]
interleave [] xs 		 = 	[xs]
interleave (x:xs) (y:ys) = 	[x:s | s <- (interleave xs (y:ys))] ++ 
							[y:s | s <- (interleave (x:xs) ys)]

-- Problem 5. (medium) Write a function that takes a list as argument and 
-- returns a list containing all permutations of the input list
permutations' :: [a] -> [[a]]
permutations' [] = [[]]
permutations' xs = [ y:zs | (y,ys) <- pick xs, zs <- permutations' ys]
  where pick []     = []
    	pick (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- pick xs ]


-- Problem 6. (easy-medium) Implement mergesort.
merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] xs = xs
merge (x:xs) (y:ys) = if x <= y
					then [x] ++ merge xs (y:ys)
					else [y] ++ merge (x:xs) ys

mergesort :: [Int] -> [Int]
mergesort [] 	= []
mergesort [x] 	= [x]
mergesort xs 	= merge (mergesort left) (mergesort right)
	where (left, right) = split xs

split :: [Int] -> ([Int], [Int])
split [] 		= ([],[])
split [x] 		= ([x],[])
split (x:y:ys) 	= (x:left, y:right)
	where (left, right) = split ys

-- What is the (average case and worst case) complexity of your code?
-- Both the worst case run time and the average is n*lg(n)
-- Mergesort divides the list in half until the individual pieces have
-- one or no elements.  This means it must make lg(n) divisions.  In each
-- of those divisions it must iterate through 2^n lists consisting of 
-- n/(2^n elements). (sum from 1 to lg(n) of (2^n)*n/(2^n))


-- Problem 7. (easy) Implement quickselect: return the kth smallest element of a 
-- list.  (see CLRS, Section 9.2, for details if you're not familiar with this)
quickselect :: Int -> [Int] -> Int
quickselect k xs = 
	if k > length xs
		then error "List can not be empty"
		else quickselect' k xs

quickselect' :: Int -> [Int] -> Int
quickselect' k (x:xs)
	| len 			>= k = quickselect' k small
	| len + 1 		== k = x
	| otherwise			 = quickselect' (k-len-1) big
	where 
		small 	= [ n | n <- xs, n <  x ]
		big 	= [ n | n <- xs, n >= x ]
		len 	= length small	

-- What is the (average case and worst case) complexity of your code?
-- The worst case is O(n^2), this can happen in a similar manner ot quicksort,
-- if bad pivots are selected, you must iterate through the list n times
-- and the size of the list only decreases by 1 each time.
-- (sum from i =1 to n of (n-i)) 
-- The average case complexity is O(n), you will divide the list lg(n) total
-- times, and each time you must iterate through a list whose size is cut in
-- half after iteration (sum from i = 1 to lg(n) of n/i), resulting in linear
-- time


-- 8. (in your own words) Compared to imperative programming, what aspects of 
-- functional programming do you like the most, and what do you dislike the
-- most?  The code is clean and elegant.  I like how the code resembles 
-- mathematical definitions.  List comprehensions look like set comprehensions 
-- in math.  The guards look like you are defining a function.  Fewer if 
-- statements. Code is concise and not as messy.
-- What I didn't like was the increased difficulty of properly analyzing my
-- code.  In a procedural language, it is much easier to accomplish this, since
-- you are giving instructions to the computer.  With functional programming, 
-- you have to know more about what is going on under the hood in order to 
-- properly analyze.  For example, a list comprehension looks like a simple
-- defintion as everything else, but it requires the computer to iterate through 
-- the list.  It also seemed harder to handle errors from improper input since 
-- so much recursion is being used.

-- Debriefing (required!): --------------------------

-- 1. Approximately how many hours did you spend on this assignment?
--		About 10 hours, probably 5 of which were spent reading the haskell book, 
--		the rest were spent doing the assignment.
-- 2. Would you rate it as easy, moderate, or difficult?
-- A. 	I would say it is moderate difficulty for a beginner.  Nothing really 
--		felt too far out of reach and it was nothing that couldn't be solved  
--		without spending some time and doing research.
-- 3. Did you work on it mostly alone, or mostly with other people?
--		Alone.
-- 4. How deeply do you feel you understand the material it covers (0%â€“100%)?
--		90%, I am still unsure of the finer points like analyzing the functions  
-- 5. Any other comments?
--		

-- This section is intended to help us calibrate the homework assignments. 
-- our answers to this section will *not* affect your grade; however, skipping
-- it will certainly do.

