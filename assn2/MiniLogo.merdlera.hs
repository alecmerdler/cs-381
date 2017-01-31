module MiniLogo where


--
-- * Part 1: Define the abstract syntax of MiniLogo as a set of Haskell data types.
--

-- | Pen status
data Mode = Up
          | Down
          deriving (Show,Eq)


-- | Different expressions
data Expr = Ref String
          | Num
          | Add Expr Expr
          deriving (Eq,Show)


-- | Macro body (list of commands)
type Prog = [Cmd]


-- | Standard commands in MiniLog
data Cmd = Pen Mode
         | Move (Expr, Expr)
         | Define String [String] Prog
         | Call Cmd [Expr]
         deriving Show


--
-- * Part 2: Define a MiniLogo macro line (x1,y1,x2,y2) that (starting from anywhere on the canvas)
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
-- * Part 3: Use the line macro defined above to define a new MiniLogo macro nix (x,y,w,h) that
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
      , Call line [(Ref "x"), (Ref "y"), Add (Ref "x") (Ref "w"), Add (Ref "y") (Ref "h")]
      , Pen Up
      , Call line [Add (Ref "x") (Ref "w"), (Ref "y"), (Ref "x"), Add (Ref "y") (Ref "h")]
      ]


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