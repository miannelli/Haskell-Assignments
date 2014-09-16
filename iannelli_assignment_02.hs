--Programming Languages, Fall 2013
--HW2 - More Haskell
--Due electronically on Blackboard on Monday Sep 16, 11:59pm.

--Expected Amount of Work for an average student: 6-8 hours.

--Textbooks for References:
--[1] Learn You a Haskell for Great Good:
--http://learnyouahaskell.com/chapters

--If you have questions, consult slides and Chapter 8 of [1]; note that much of 
--the latter is too advanced for this HW.

--In this HW we will build a lexer and parser (in case you're not familiar with 
--these concepts, read "lexical analysis" from wikipedia) for simple arithmetic
--expressions. Familiarity with these concepts and techniques are extremely 
--important for the rest of this course and your understanding of PL in general.
import Data.Char

--0. (easy-medium) 

--   (a) Implement a tail recursive factorial function.
factorial x = factorial' x 1

factorial' 0 r = r
factorial' x r = factorial' (x-1) (r*x)
--   (b) Implement a recursive but linear-time fibonacci function using 
--       the idea of "tail-recursion" as in the reverse function from the slides.
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci x  
  | x <= 1 = error "Fibonacci undefined for values less than 1"   
  | otherwise = fibonacci' (x-2) 1 1

fibonacci' 0 value _ = value
fibonacci' count v1 v2 = fibonacci' (count-1) (v1+v2) v1


--1. Building a Lexer. Consider the following datatype of tokens:

data Token = Number Int
           | Plus
           | Minus
           | Times
           | LParen
           | RParen 
             deriving (Show, Eq)

--Note that: 
--a. "deriving (Show)" is like defining "toString" method in Java or "__str__" method in Python,
--just for your debugging convenience. Eq is like __eq__.
--b. we'll be using Int instead of Integer for easy conversion from Char to Int. You'll need:

--   Prelude> import Data.Char
--   Prelude> digitToInt '5'
--   5
   

--Write a function mylex that takes a string as input and produces a list of tokens as output.
--Your function should:
-- * map consecutive sequences of digits to appropriate instances of the Number constructor
-- * map the characters '+', '-', '*', '(', and ')' to Plus, Minus, Times, LParen, and RParen, resp.
-- * ignore whitespace (' ')
-- * fail (by raising the exception Bad) on all other characters (simply: error "Bad").

--Examples:

-- *Main> mylex " (12 +340) "
-- [LParen,Number 12,Plus,Number 340,RParen]

-- *Main> mylex "a"
-- *** Exception: Bad

-- *Main> mylex []
-- []

-- *Main> mylex "((12+340)*  )"
-- [LParen,LParen,Number 12,Plus,Number 340,RParen,Times,RParen]

mylex :: [Char] -> [Token]
mylex [] = []
mylex (x:xs)
  | elem x ['0'..'9'] = lexMultiChar [x] xs
  | x == '+'          = Plus                    : (mylex xs)
  | x == '-'          = Minus                   : (mylex xs)
  | x == '*'          = Times                   : (mylex xs)
  | x == '('          = LParen                  : (mylex xs)
  | x == ')'          = RParen                  : (mylex xs)
  | x == ' '          =                           (mylex xs)
  | otherwise         = error "Bad"

lexMultiChar :: [Char] -> [Char] -> [Token]
lexMultiChar str [] = [(Number (read (reverse str) :: Int))]
lexMultiChar str (x:xs)
  | elem x ['0'..'9'] = lexMultiChar (x:str) xs
  | otherwise         = (Number (read (reverse str) :: Int)): (mylex (x:xs))


-- 2. Building a Parser. 

-- Here is a very simple grammar of fully parenthesized arithmetic expressions,
     
--     exp ::= number | (exp + exp) | (exp âˆ’ exp) | (exp âˆ— exp)

-- This notation is called Backus-Naur Form (BNF) or context-free grammar (CFG).
-- These concepts will be used throughout this course;
-- read wikipedia if you're not familiar with them.

-- Here is a datatype definition representing the corresponding type of abstract syntax trees (which we saw in class).

data Ast = ANum Int
         | APlus Ast Ast
         | ATimes Ast Ast
         | AMinus Ast Ast  
           deriving (Show, Eq)

-- Write a function parse that takes a list l of tokens and produces a pair (e,lâ€™),
-- where e is a value of type ast (following the above grammar) and lâ€™ is a list of
-- tokens representing the portion of l that was left over after parsing e. 
-- Your function should raise the exception "Bad" if the token list does not correspond
-- to a legal expression.

-- Examples:

-- *Main> parse [Number 5]
-- (ANum 5,[])

-- *Main> parse [Number 5, Times, Number 6]
-- (ANum 5,[Times,Number 6])

-- *Main> parse [LParen, Number 3, Times, LParen, Number 5, Plus, Number 6, RParen, RParen, Times]
-- (ATimes (ANum 3) (APlus (ANum 5) (ANum 6)),[Times])

-- *Main> parse [LParen, Number 5]
-- *** Exception: Bad

-- Hint: if checking whether an expression is valid is too hard, you can ignore the exception part
-- and assume the input is valid for the time-being.
parse :: [Token] -> (Ast, [Token])
parse ((Number y):xs) = (ANum y, xs)
parse (LParen:xs)     = ((opToNode op) left right, remainder)
  where (left, op:rest) = parse xs
        (right, RParen:remainder) = parse rest

opToNode :: Token -> Ast -> Ast -> Ast
opToNode x 
  | x == Times   = ATimes
  | x == Plus   = APlus
  | x == Minus  = AMinus
  | otherwise   = error "Bad"


--3. Put all of the pieces together: take the eval function given in lecture together 
--with your lex and parse functions and write a function calc that takes a string and 
--returns an integer. If the string represents a valid arithmetic expression, calc function
--should return its value as computed by eval. If it is not a valid expression, it should 
--raise the exception Bad.

--For your convenience I've copied the function eval here:

eval :: Ast -> Int
eval (ANum x) = x
eval (ATimes x y) = (eval x) * (eval y)
eval (APlus x y) = (eval x) + (eval y)
eval (AMinus x y) = (eval x) - (eval y)

--Examples:

-- *Main> calc "((1+2)*3)"
--9

-- *Main> calc "(1+2) 5"
-- *** Exception: Bad

-- *Main> calc "((2+1) * (11+8))"
--57

calc :: [Char] -> Int
calc = eval . fst . parse . mylex


--Debriefing (required!): --------------------------

--1. Approximately how many hours did you spend on this assignment?
--      Approximately 8-10 hours
--2. Would you rate it as easy, moderate, or difficult?
--      Difficult
--3. Did you work on it mostly alone, or mostly with other people?
--      Alone
--4. How deeply do you feel you understand the material it covers (0%â€“100%)?
--      I am still a bit confused on the parser 
--5. Any other comments?


--This section is intended to help us calibrate the homework assignments. 
--Your answers to this section will *not* affect your grade; however, skipping it
--will certainly do.
