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
cmd :: Cmd -> State -> (State, Line)
cmd (Pen a) (b, x, y) = ((a, x, y), ((0, 0), (0, 0)))
