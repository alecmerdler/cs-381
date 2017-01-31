module MiniLogo where


--
-- * Part 1: Define the abstract syntax of MiniLogo as a set of Haskell data types.
--

-- | Pen status
data Mode = Up
          | Down
          deriving (Show, Eq)


-- | Different expressions
data Expr = String
          | Num
          | Expr + Expr
          deriving Show


-- | Standard commands in MiniLog
data Cmd = Pen Mode
         | Move Int Int
         | Define String
         | Call String
         deriving Show


--
-- * Part 2: Define a MiniLogo macro line (x1,y1,x2,y2) that (starting from anywhere on the canvas)
--           draws a line segment from (x1,y1) to (x2,y2).
--




--
-- * Part 3: Use the line macro defined above to define a new MiniLogo macro nix (x,y,w,h) that
--           draws a big “X” of width w and height h, starting from position (x,y).
--           Your definition should not contain any move commands.
--



--
-- * Part 4: Define a Haskell function steps :: Int -> Prog that constructs a MiniLogo
--           program that draws a staircase of n steps starting from (0,0).
--



--
-- * Part 5: Define a Haskell function macros :: Prog -> [Macro] that returns a list of the names
--           of all of the macros that are defined anywhere in a given MiniLogo program.
--


--
-- * Part 6: Define a Haskell function pretty :: Prog -> String that pretty-prints a MiniLogo program.
--