-- Michael Iannelli
-- Programming Languages, Fall 2014
-- HW4 - Lambda Calculus I; Lazy Evaluation
-- Due electronically on Blackboard on Tuesday Oct 7, 2pm.

-- Expected Amount of Work for an average student: 6 hours.

-- Textbooks for References:
-- [1] TAPL, Chap. 5

-- 1. Finish and extend part 2 of the Quiz:

data Ast = TRUE
         | FALSE
         | IFTHENELSE Ast Ast Ast
           deriving (Show, Eq)

-- implement three functions: 
-- a) eval (for one-step evaluation)
eval' :: Ast -> Ast
eval' (IFTHENELSE TRUE t1 t2) = t1
eval' (IFTHENELSE FALSE t1 t2) = t2
eval' (IFTHENELSE t1 t2 t3) = IFTHENELSE (eval' t1) t2 t3

eval :: Ast -> Ast
eval TRUE = TRUE
eval FALSE = FALSE
eval t = eval' t

-- b) eval2 (for multi-step evaluation)
eval2 :: Ast -> Ast
eval2 TRUE = TRUE
eval2 FALSE = FALSE
eval2 t = eval2 (eval t)

-- c) eval3 (for big-step evaluation)
eval3 :: Ast -> Ast
eval3 TRUE = TRUE
eval3 FALSE = FALSE
eval3 (IFTHENELSE t1 t2 t3)
  | eval3 t1  == TRUE   =   eval3 t2
  | otherwise           =   eval3 t3

--Note that all three functions have type Ast -> Ast.

--Show a few testcases (by continuing calling eval on some term you'll eventually get the same answer 
--as eval2 and eval3).

-- *Main>  eval (IFTHENELSE (IFTHENELSE TRUE TRUE FALSE) (IFTHENELSE TRUE TRUE FALSE) TRUE)
-- IFTHENELSE TRUE (IFTHENELSE TRUE TRUE FALSE) TRUE
-- *Main>  eval (IFTHENELSE TRUE (IFTHENELSE TRUE TRUE FALSE) TRUE)
-- IFTHENELSE TRUE TRUE FALSE
-- *Main> eval (IFTHENELSE TRUE TRUE FALSE)
-- TRUE

-- *Main> eval2 (IFTHENELSE (IFTHENELSE TRUE TRUE FALSE) (IFTHENELSE TRUE TRUE FALSE) TRUE)
-- TRUE

-- *Main> eval3 (IFTHENELSE (IFTHENELSE TRUE TRUE FALSE) (IFTHENELSE TRUE TRUE FALSE) TRUE)
-- TRUE

-- *Main> eval (IFTHENELSE FALSE TRUE (IFTHENELSE FALSE TRUE TRUE))
-- IFTHENELSE FALSE TRUE TRUE
-- *Main> eval (IFTHENELSE FALSE TRUE TRUE)
-- TRUE

-- *Main> eval2 (IFTHENELSE FALSE TRUE (IFTHENELSE FALSE TRUE TRUE))
-- TRUE

-- *Main> eval3 (IFTHENELSE FALSE TRUE (IFTHENELSE FALSE TRUE TRUE))
-- TRUE


--2. Redefine the one-step evaluation to be call-by-name (i.e., lazy) to
--   simulate Haskell. 

--3. Define the call-by-value big-step evaluation for lambda calculus. 
--   Define the call-by-name  big-step evaluation for lambda calculus. 

--4. What does \x -> \x -> x + 1 do? (is it a valid lambda calculus expression?)

--5. In the class we showed an example of Haskell lazy evaluation:

--      (\x -> 3) [1..100000000000000000000]

--   But this example is kind of trivial. Can you find a more interesting example,
--   in the sense that the argument x is indeed used in the function, but nevertheless
--   you still don't need to compute x before applying the function?

--   Hint: lazy list [1..]

--6. In the class we also demonstrated that Python is _not_ lazy:

--      (lambda x: 3) (range(100000000000000000))

--   Is there a workaround that can simulate call-by-name in Python?

--7. Consider the "double" operator from the slides:

--       double = \f -> \y -> f (f y)

--   Is it a binary function (taking f and y) or a unary function?
--   How do you understand the statement that all binary functions 
--   can be represented as unary functions of unary functions?

--   And what's the type of double?

--8. How do make a nullary (0-ary) function in Haskell and Python, respectively?
--   What are their types?

--Debriefing (required!): --------------------------

--1. Approximately how many hours did you spend on this assignment?
--2. Would you rate it as easy, moderate, or difficult?
--3. Did you work on it mostly alone, or mostly with other people?
--4. How deeply do you feel you understand the material it covers (0%â€“100%)? 
--5. Any other comments?

--This section is intended to help us calibrate the homework assignments. 
--Your answers to this section will *not* affect your grade; however, skipping it
--will certainly do.