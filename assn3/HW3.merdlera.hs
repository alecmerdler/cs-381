module HW3 where

import Render
import MiniMiniLogo


-- | Represents the state of the pen
type State = (Mode, Int, Int)


--
-- | Part I: Implement cmd, the semantic function for MiniMiniLogo commands (Cmd).
--
-- >>> cmd (Pen Up) (Down, 0, 0)
-- ((Up,0,0),((0,0),(0,0)))
--
-- >>> cmd (Move 1 1) (Down, 0, 0)
-- ((Down,1,1),((0,0),(1,1)))
--
cmd :: Cmd -> State -> (State, Line)
cmd (Pen a) (_, x, y)        = ((a, x, y), ((0, 0), (0, 0)))
cmd (Move x2 y2) (b, x1, y1) = ((b, x2, y2), ((x1, y1), (x2, y2)))


--
-- | Part II: Implement prog, the semantic function for MiniMiniLogo programs (Prog).
--
-- >>> prog [] (Up, 0, 4)
-- ((Up,0,4),[((0,0),(0,0))])
--
-- >>> prog [(Pen Up)] (Down, 0, 0)
-- ((Up,0,0),[((0,0),(0,0))])
--
-- >>> prog [(Move 1 1)] (Down, 0, 0)
-- ((Down,1,1),[((0,0),(1,1))])
--
-- >>> prog [(Move 1 2), (Pen Up)] (Down, 0, 0)
-- ((Up,1,2),[((0,0),(1,2))])
--
-- >>> prog [(Move 1 2), (Move 1 3)] (Down 0 0)
-- ((Down,1,3),[((0,0),(1,2)),((1,2),(1,3))]
--
prog :: Prog -> State -> (State, [Line])
prog [] (a, x, y)     = ((a, x, y), [((0, 0), (0, 0))])
prog [a] b            = (\(a, b) -> (a, [b])) (cmd a b)
prog (x:xs) a         = (\(a, b) -> (a, [b])) (cmd x a)


--
-- * Helper Functions
--
