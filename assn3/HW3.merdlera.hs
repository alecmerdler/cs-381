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
prog :: Prog -> State -> (State, [Line])
prog [] (a, x, y) = ((a, x, y), [((0, 0), (0, 0))])
prog [a] b = (\(a, b) -> (a, [b])) (cmd a b)
prog (a:bs) (c, x, y) = undefined


--
-- * Helper Functions
--
