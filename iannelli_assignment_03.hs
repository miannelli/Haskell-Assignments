--Michael Iannelli
--Programming Languages, Fall 2014
--HW3 - Induction and Operational Semantics
--Due electronically on Blackboard on Tuesday Sep 30, 2pm.

--Expected Amount of Work for an average student: 8 hours.

--Textbooks for References:
--[1] TAPL, Chaps. 1 and 3


--0. What's wrong with this proof?

--Theorem(?!): All horses are the same color.

-- Proof: Let P(n) be the predicate "in all non-empty collections of n horses, 
-- all the horses are the same color." 
-- We show that P (n) holds for all n by induction on n (using 1 as the base 
-- case).

--Base case: Clearly, P(1) holds.

--Induction case: Given P (n), we must show P (n + 1).

-- Consider an arbitrary collection of n + 1 horses. Remove one horse temporarily. 
-- Now we have n horses and hence, by the induction hypothesis, these n horses 
-- are all the same color. Now call the exiled horse back and send a different 
-- horse away. Again, we have a collection of n horses, which, by the induction 
-- hypothesis, are all the same color. Moreover, these n horses are the same 
-- color as the first collection. Thus, the horse we brought back was the same 
-- color as the second horse we sent away, and all the n + 1 horses are the same 
-- color.

-- A. The problem with this proof is it does not hold for all n.  
-- The proof fails for P(2), in the fact that the horses can be different colors.
-- "These n horses are the same color as the first collection" is incorrect.

--1. How about this one?

-- Theorem(?!): n^2 + n is odd for every n >= 1.

-- Proof: By induction on n (again starting from 1). For the base case, observe 
-- that 1 is odd by definition. For the induction step, assume that n^2 + n is 
-- odd; we then show that (n + 1)^2 + (n + 1) is odd as follows. 
-- (n+1)^2 +(n+1) = n^2 +2n+1+n+1 = (n^2 +n)+(2n+2). But n^2 +n is odd by the 
-- induction hypothesis, and 2n + 2 is clearly even. Thus, (n^2 + n) + (2n + 2) 
-- is the sum of an odd number and an even number, hence odd.

-- A. The proof states that 1 is odd instead of f(1), the first term.  It is an
-- incorrect base case.  f(1) = 2, which is not odd.  This changes the outcome
-- in the fact that n^2 + n is even for all n >= 1.

-- 2. Make up your own "false proof" in which an incorrect use of the induction 
-- hypothesis leads to a surprising conclusion.

-- The teacher says we will have a pop quiz one weekday next week and guarantees
-- that it will be a surprise.
-- We conclude that the teacher will not give a quiz at all.
-- Proof:
-- Base case: If we have gone till thursday weithout a pop quiz, then the quiz
-- must be friday.  But if we know it is Friday, we can not be surprised, so the
-- quiz can not be on Friday.
-- Induction: Using this same logic, we can conclude that the quiz can not be on
-- thurday if we have gone till wednesday without a quiz, then not on wednesday,
-- etc. until there are no days left.  Therefore there will be no quiz.

-- 3. What does "if iszero (succ 0) then pred (succ 0) else succ (pred 0)" 
-- evaluate to (in one step)?

-- A. if iszero false then pred (succ 0) else succ (pred 0)

--   Draw the derivation tree for that one-step evaluation.

-- A. Assuming same system as textbook:
-- 
--                                          iszero (succ 0) -> false
-- ------------------------------------------------------------------------------------------------------------ E-If
-- if iszero (succ 0) then pred (succ 0) else succ (pred 0) -> if false then pred (succ 0) else succ (pred 0)

-- 4. Exercise 3.5.13 in TAPL.
-- 3.5.4  Does not hold since 1 term can be an instantiation of 2 different rules
-- 3.5.7  Holds
-- 3.5.8  Holds
-- 3.5.11 Does not hold since you can get two different normal forms if you
--        apply different rules to the same term 
-- 3.5.12 Holds

-- 3.5.4  Does not hold since 1 term can be an instantiation of 2 different rules
-- 3.5.7  Holds
-- 3.5.8  Holds
-- 3.5.11 Holds
-- 3.5.12 Holds
-- The proof of uniqueness of normal forms (3.5.11) must be altered.  Now we
-- must show that no matter which path is taken, termination occurs at the
-- same normal form.  The diamond property is used to show this. 

-- 5. Exercise 3.5.17 in TAPL.
-- I am very confused on big step sematnics.  The answer is available,
-- but I'm not sure exactly how to use them.
-- We can prove case by case that each of the operational symatic rules
-- is equivilant to the big-step semantics rules.
-- 1. B-Value is trivial
-- 2. B-IfTrue and B-IfFalse are similar
--    
-- if TRUE then v2 else t3 -> v2
-- is equivilant to their corresponding big step rules.
-- This is obvious since they are one step.
-- This is similar for IfFalse and derivations.

-- 

-- 6. (from slides) We want to change the evaluation strategy so that 
-- the then and else branches of an if get evaluated (in that order) 
-- before the guard. Furthermore, if the evaluation of the then and else 
-- branches leads to the same value, we want to immediately produce that 
-- value and short-circuit the evaluation of the guard.

-- Produce a new (complete) set of semantic rules for the above.

-- if t then t1 else t1 -> t1

--              t2 -> t2'    t3 -> t3'
-- ----------------------------------------------------------
-- if t1 then t2 else t3 -> if t1 then t2' else t3'

--                    t1 -> t1'
-- ----------------------------------------------------------
-- if t1 then TRUE else FALSE -> if t1' then TRUE else FALSE


--                    t1 -> t1'
-- ----------------------------------------------------------
-- if t1 then FALSE else TRUE -> if t1' then FALSE else TRUE

-- Now redo this question using big-step semantics.
-- A. I'm not really sure how to answer this.  Both the book and notes only really
-- have one page on big-step semantics without examples of how to use it.


-- 7. In Arithmetic Expressions (NB, Fig 3-2), show me a "stuck term" without using "if".
--   How to modify the syntax to disallow such bad terms?

-- A. "iszero false" is a stuck term
-- A. Add a new set of terms to handle these errors.
-- For example badnat = {wrong, true, false}.  Add a new set
-- of evaluation relations that evaluates the stuck terms to wrong.
-- For example "iszero badnat" evaluates to "wrong". 

-- 8. Implement in Haskell the "if then else" language (i.e., only booleans, no numbers),
--   its one-step, multi-step, and big-step evalution.

--   For example, calling the one-step eval function
   
--   eval (IFTHENELSE TRUE FALSE TRUE)
   
--   will return
   
--   FALSE

--   Make sure your multi-step and big-step return the same results.

data Ast  = TRUE
   	      | FALSE
          | IFTHENELSE Ast Ast Ast
          deriving (Show, Eq)

-- One-Step evaluation
eval (IFTHENELSE TRUE t1 t2) = t1
eval (IFTHENELSE FALSE t1 t2) = t2
eval (IFTHENELSE t1 t2 t3) = IFTHENELSE (eval t1) t2 t3

-- Multi-Step evaluation
mseval TRUE = TRUE
mseval FALSE = FALSE
mseval t = mseval (eval t) 

-- Big-Step Evaluation
bseval TRUE = TRUE
bseval FALSE = FALSE
bseval (IFTHENELSE t1 t2 t3)
  | bseval t1 == TRUE   = bseval t2
  | otherwise           = bseval t3


-- Debriefing (required!): --------------------------

--1. Approximately how many hours did you spend on this assignment?
--  About 20 hours if you include reading, 7 or so on the assignment.
--2. Would you rate it as easy, moderate, or difficult?
-- Difficult.
--3. Did you work on it mostly alone, or mostly with other people?
-- With other people.
--4. How deeply do you feel you understand the material it covers (0%â€“100%)? 
-- 75%.  I do not understand big step semantics.  The book and notes do not really
-- go into them in detail or have examples of how to use them.
--5. Any other comments?

--This section is intended to help us calibrate the homework assignments. 
--Your answers to this section will *not* affect your grade; however, skipping it
--will certainly do.