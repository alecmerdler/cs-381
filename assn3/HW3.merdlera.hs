module HW3 (State, cmd, prog) where

import Render
import MiniMiniLogo


--
-- * Semantics of MiniMiniLogo
--

-- NOTE:
--   * MiniMiniLogo.hs defines the abstract syntax of MiniMiniLogo and some
--     functions for generating MiniMiniLogo programs. It contains the type
--     definitions for Mode, Cmd, and Prog.
--   * Render.hs contains code for rendering the output of a MiniMiniLogo
--     program in HTML5. It contains the types definitions for Point and Line.

-- | A type to represent the current state of the pen.
type State = (Mode,Point)

-- | The initial state of the pen.
start :: State
start = (Up,(0,0))

-- | A function that renders the image to HTML. Only works after you have
--   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
--   produce an HTML file named MiniMiniLogo.html, which you can load in
--   your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p = let (_,ls) = prog p start in toHTML ls


-- Semantic domains:
--   * Cmd:  State -> (State, Maybe Line)
--   * Prog: State -> (State, [Line])


-- | Semantic function for Cmd.
--
--   >>> cmd (Pen Down) (Up,(2,3))
--   ((Down,(2,3)),Nothing)
--
--   >>> cmd (Pen Up) (Down,(2,3))
--   ((Up,(2,3)),Nothing)
--
--   >>> cmd (Move 4 5) (Up,(2,3))
--   ((Up,(4,5)),Nothing)
--
--   >>> cmd (Move 4 5) (Down,(2,3))
--   ((Down,(4,5)),Just ((2,3),(4,5)))
--
--   >>> cmd (Pen Up) (Down, (0, 0))
--   ((Up,(0,0)),Nothing)
--
--   >>> cmd (Move 1 1) (Down, (0, 0))
--   ((Down,(1,1)),Just ((0,0),(1,1)))
--
cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen a) (_, (x, y))        = ((a, (x, y)), Nothing)
cmd (Move x2 y2) (mode, (x1, y1)) = case mode of
                                        Up   -> ((mode, (x2, y2)), Nothing)
                                        Down -> ((mode, (x2, y2)), Just ((x1, y1), (x2, y2)))


-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
--
--   >>> prog [] (Up, (0, 4))
--   ((Up,(0,4)),[])
--
--   >>> prog [(Pen Up)] (Down, (0, 0))
--   ((Up,(0,0)),[])
--
--   >>> prog [(Move 1 1)] (Down, (0, 0))
--   ((Down,(1,1)),[((0,0),(1,1))])
--
--   >>> prog [(Move 1 2), (Pen Up)] (Down, (0, 0))
--   ((Up,(1,2)),[((0,0),(1,2))])
--
--   >>> prog [(Move 1 2), (Move 1 3)] (Down, (0, 0))
--   ((Down,(1,3)),[((0,0),(1,2)),((1,2),(1,3))])
--
prog :: Prog -> State -> (State, [Line])
prog [] state   = (state, [])
prog cmds state = applyCmds cmds (state, [])


--
-- * Helper Functions
--

--
-- | Recursively applies a list of commands while updating pen state and accummulating drawn lines.
--
--   >>> applyCmds [(Pen Up)] ((Down, (0, 1)), [])
--   ((Up,(0,1)),[])
--
--   >>> applyCmds [(Move 2 2)] ((Down, (0, 0)), [])
--   ((Down,(2,2)),[((0,0),(2,2))])
--
--   >>> applyCmds [(Move 2 2), (Move 3 3)] ((Down, (1, 0)), [((0,0),(2,2))])
--   ((Down,(3,3)),[((0,0),(2,2)),((1,0),(2,2)),((2,2),(3,3))])
--
--   >>> applyCmds [Pen Up,Move 10 10,Pen Down,Move 15 17,Pen Up,Move 10 17,Pen Down,Move 15 10] ((Up, (0, 0)), [])
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
applyCmds :: [Cmd] -> (State, [Line]) -> (State, [Line])
applyCmds [] result               = result
applyCmds (c:cmds) (state, lines) = case line of
                                        Just l  -> applyCmds cmds (newState, lines ++ [l])
                                        Nothing -> applyCmds cmds (newState, lines)
                                    where (newState, line) = cmd c state


--
-- * Extra credit
--

-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.
--
--   Draws "CS381"
--
-- >>> prog amazing (Up, (0, 0))
-- ((Down,(16,20)),[((3,23),(0,23)),((0,23),(0,20)),((0,20),(3,20)),((7,23),(4,23)),((4,23),(4,22)),((4,22),(7,22)),((7,22),(7,20)),((7,20),(4,20)),((8,23),(11,23)),((11,23),(11,22)),((11,22),(8,22)),((8,22),(11,22)),((11,22),(11,20)),((11,20),(8,20)),((15,22),(15,23)),((15,23),(12,23)),((12,23),(12,22)),((12,22),(15,22)),((15,22),(15,20)),((15,20),(12,20)),((12,20),(12,22)),((16,23),(16,20))])
--
-- >>> draw amazing
--
--
amazing :: Prog
amazing = liftPen (3, 23) ++
          [Move 0 23, Move 0 20, Move 3 20] ++
          liftPen (7, 23) ++
          [Move 4 23, Move 4 22, Move 7 22, Move 7 20, Move 4 20] ++
          liftPen (8, 23) ++
          [Move 11 23, Move 11 22, Move 8 22, Move 11 22, Move 11 20, Move 8 20] ++
          liftPen (15, 22) ++
          [Move 15 23, Move 12 23, Move 12 22, Move 15 22, Move 15 20, Move 12 20, Move 12 22] ++
          liftPen (16, 23) ++
          [Move 16 20]


--
-- * Helper Functions
--

--
-- | Returns a set of commands that moves the pen to the given coordinates without drawing a line,
--   then puts the pen down.
--
--   >> liftPen (1, 0)
--   [Pen Up,Move 1 0,Pen Down]
--
--   >> prog (liftPen (1, 0)) (Up, (7, 7))
--   ((Down,(1,0)),[])
--
liftPen :: (Int, Int) -> Prog
liftPen (x, y) = [Pen Up, Move x y, Pen Down]
