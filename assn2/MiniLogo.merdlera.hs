module MiniLogo where

import Prelude hiding (Num)
import Data.List

--
-- | Part 1: Define the abstract syntax of MiniLogo as a set of Haskell data types.
--

-- | Primitive MiniLog types, using type synonyms
type Num = Int
type Var = String
type Macro = String


-- | Pen status
data Mode = Up
          | Down
          deriving (Show,Eq)


-- | MiniLog expressions
--   Ref Var       - A reference to a string variable name
--   Lit Num       - A literal integer
--   Add Expr Expr - Adding two expressions
data Expr = Ref Var
          | Lit Num
          | Add Expr Expr
          deriving (Eq,Show)


-- | Macro body (list of commands)
type Prog = [Cmd]


-- | Standard commands in MiniLog
data Cmd = Pen Mode
         | Move (Expr, Expr)
         | Define Macro [Var] Prog
         | Call Macro [Expr]
         deriving Show


--
-- | Part 2: Define a MiniLogo macro line (x1,y1,x2,y2) that (starting from anywhere on the canvas)
--           draws a line segment from (x1,y1) to (x2,y2).
--
--           MiniLog concrete syntax:
--
--           define line (x1, y1, x2, y2) {
--               pen up; move (x1, y1);
--               pen down; move (x2, y2);
--           }
--
line :: Cmd
line = Define "line" ["x1", "y1", "x2", "y2"]
       [ Pen Up
       , Move (Ref "x1", Ref "y1")
       , Pen Down, Move (Ref "x2", Ref "y2")
       ]


--
-- | Part 3: Use the line macro defined above to define a new MiniLogo macro nix (x,y,w,h) that
--           draws a big “X” of width w and height h, starting from position (x,y).
--           Your definition should not contain any move commands.
--
--           MiniLog concrete syntax:
--
--           define nix (x, y, w, h) {
--               pen down; line (x, y, x + w, y + h);
--               pen up; line (x + w, y, x, y + h);
--           }
--
nix :: Cmd
nix = Define "nix" ["x", "y", "w", "h"]
      [ Pen Down
      , Call "line" [(Ref "x"), (Ref "y"), Add (Ref "x") (Ref "w"), Add (Ref "y") (Ref "h")]
      , Pen Up
      , Call "line" [Add (Ref "x") (Ref "w"), (Ref "y"), (Ref "x"), Add (Ref "y") (Ref "h")]
      ]


--
-- | Part 4: Define a Haskell function steps :: Int -> Prog that constructs a MiniLogo
--           program that draws a staircase of n steps starting from (0,0).
--
-- >>> steps 0
-- [Pen Up,Move (Lit 0,Lit 0),Pen Down]
--
-- >>> steps 1
-- [Pen Up,Move (Lit 0,Lit 0),Pen Down,Move (Lit 0,Lit 1),Move (Lit 1,Lit 1)]
--
steps :: Int -> Prog
steps 0   = [Pen Up, Move (Lit 0, Lit 0), Pen Down]
steps num = steps (pred num) ++ [Move ((Lit (pred num)), (Lit num))] ++ [Move ((Lit num), (Lit num))]


--
-- | Part 5: Define a Haskell function macros :: Prog -> [Macro] that returns a list of the names
--           of all of the macros that are defined anywhere in a given MiniLogo program.
--
-- >>> macros []
-- []
--
-- >>> macros [Pen Up]
-- []
--
-- >>> macros [Pen Up, Define "test" [] []]
-- ["test"]
--
-- >>> macros [Pen Up, Define "test" [] [], Pen Down, Define "test2" [] []]
-- ["test","test2"]
--
macros :: Prog -> [Macro]
macros []          = []
macros (x:xs)
        | (ismacro x) == True = macroName x : macros xs
        | otherwise           = macros xs


-- ** Helper functions **

-- | Determines if the given MiniLog command is a macro definition.
ismacro :: Cmd -> Bool
ismacro (Define _ _ _) = True
ismacro _              = False


-- | Returns the macro name of a given macro definition
macroName :: Cmd -> Macro
macroName (Define m _ _) = m


--
-- | Part 6: Define a Haskell function pretty :: Prog -> String that pretty-prints a MiniLogo program.
--
-- >>> pretty []
-- ""
--
-- >>> pretty [Pen Up]
-- "pen up;\n"
--
-- >>> pretty [(Define "test" ["x", "y"] [Pen Up]), (Pen Down), (Call "line" [(Ref "x")])]
-- "define test(x, y) {\npen up;\n}\npen down;\ncall line (x);\n"
--
pretty :: Prog -> String
pretty []     = ""
pretty (x:xs) = prettifyCmd x ++ pretty xs


-- ** Helper functions **

-- | Returns the string representation of a given abstract MiniLog command.
--
-- >>> prettifyCmd (Pen Up)
-- "pen up;\n"
--
-- >>> prettifyCmd (Pen Down)
-- "pen down;\n"
--
-- >>> prettifyCmd (Move ((Ref "x"), (Lit 7)))
-- "move (x, 7);\n"
--
-- >>> prettifyCmd (Call "line" [(Lit 1), (Ref "x"), (Add (Ref "x") (Lit 4))])
-- "call line (1, x, x + 4);\n"
--
-- >>> prettifyCmd (Define "test" ["x", "y"] [Pen Up, Pen Down])
-- "define test(x, y) {\npen up;\npen down;\n}\n"
--
prettifyCmd :: Cmd -> String
prettifyCmd (Pen Up)      = "pen up;\n"
prettifyCmd (Pen Down)    = "pen down;\n"
prettifyCmd (Move (a, b)) = "move (" ++ prettifyExpr a ++ ", " ++ prettifyExpr b ++ ");\n"
prettifyCmd (Call a b)    = "call " ++ a ++ " (" ++ concat (intersperse ", " (map prettifyExpr b)) ++ ");\n"
prettifyCmd (Define a b c) = "define " ++ a ++ "(" ++ prettifyVars b ++ ") {\n" ++ pretty c ++ "}\n"


-- | Returns the string representation of a given abstract MiniLog expression.
--
-- >>> prettifyExpr (Ref "x")
-- "x"
--
-- >>> prettifyExpr (Lit 9)
-- "9"
--
-- >>> prettifyExpr (Add (Lit 9) (Ref "x"))
-- "9 + x"
--
prettifyExpr :: Expr -> String
prettifyExpr (Ref x)   = x
prettifyExpr (Lit x)   = show x
prettifyExpr (Add x y) = (prettifyExpr x) ++ " + " ++ (prettifyExpr y)


-- | Returns the string representation of a given list of abstract MiniLog variables.
--
-- >>> prettifyVars []
-- ""
--
-- >>> prettifyVars ["x"]
-- "x"
--
-- >>> prettifyVars ["x", "y"]
-- "x, y"
--
prettifyVars :: [Var] -> String
prettifyVars []     = ""
prettifyVars [x]    = x
prettifyVars (x:xs) = x ++ ", " ++ prettifyVars xs


--
-- | Bonus - Part 7: Define a Haskell function optE :: Expr -> Expr that partially evaluates expressions by
--                   replacing any additions of literals with the result.
--
-- >>> optE (Ref "x")
-- Ref "x"
--
-- >>> optE (Lit 9)
-- Lit 9
--
-- >>> optE (Add (Lit 5) (Lit 2))
-- Lit 7
--
-- >>> optE (Add (Lit 5) (Ref "x"))
-- Add (Lit 5) (Ref "x")
--
-- >>> optE (Add (Ref "x") (Lit 8))
-- Add (Ref "x") (Lit 8)
--
-- >>> optE (Add (Add (Lit 2) (Lit 3)) (Ref "x"))
-- Add (Lit 5) (Ref "x")
--
optE :: Expr -> Expr
optE (Ref a)               = Ref a
optE (Lit a)               = Lit a
optE (Add (Lit a) (Lit b)) = Lit (a + b)
optE (Add (Lit a) (Ref b)) = Add (Lit a) (Ref b)
optE (Add (Ref a) (Lit b)) = Add (Ref a) (Lit b)
optE (Add a b)             = Add (optE a) (optE b)


--
-- | Bonus - Part 8: Define a Haskell function optP :: Prog -> Prog that optimizes all of the expressions
--                   contained in a given program using optE.
--
-- >>> optP []
-- []
--
-- >>> optP [Pen Up]
-- [Pen Up]
--
-- >>> optP [(Call "test" [(Add (Add (Lit 2) (Lit 3)) (Ref "x"))])]
-- [Call "test" [Add (Lit 5) (Ref "x")]]
--
optP :: Prog -> Prog
optP [] = []
optP (x:xs) = optC x : optP xs


-- ** Helper Functions **

-- | Optimizes a given list of MiniLog abstract expressions.
--
-- >>> optEs []
-- []
--
-- >>> optEs [(Add (Add (Lit 2) (Lit 3)) (Ref "x"))]
-- [Add (Lit 5) (Ref "x")]
--
optEs :: [Expr] -> [Expr]
optEs []     = []
optEs [x]    = [optE x]
optEs (x:xs) = optE x : optEs xs


-- | Optimizes a given MiniLog abstract command.
--
-- >>> optC (Pen Up)
-- Pen Up
--
-- >>> optC (Move ((Lit 5), (Lit 2)))
-- Move (Lit 5,Lit 2)
--
-- >>> optC (Call "test" [(Add (Add (Lit 2) (Lit 3)) (Ref "x"))])
-- Call "test" [Add (Lit 5) (Ref "x")]
--
optC :: Cmd -> Cmd
optC (Move (a, b))    = Move ((optE a), (optE b))
optC (Call a (x:xs))  = Call a (optE x : optEs xs)
optC a                = a
