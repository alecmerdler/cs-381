module HW3 where

import Render
import MiniMiniLogo


-- | Represents the state of the pen
type State = (Mode, Int, Int)

-- | Represents no line drawn
noLine = ((0, 0), (0, 0))


--
-- | Part I: Implement cmd, the semantic function for MiniMiniLogo commands (Cmd).
--
-- >>> cmd (Pen Up) (Down, 0, 0)
-- ((Up,0,0),Nothing)
--
-- >>> cmd (Move 1 1) (Down, 0, 0)
-- ((Down,1,1),Just ((0,0),(1,1)))
--
cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen a) (_, x, y)        = ((a, x, y), Nothing)
cmd (Move x2 y2) (b, x1, y1) = ((b, x2, y2), Just ((x1, y1), (x2, y2)))


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
prog [] state          = (state, [])
prog (head:tail) state = prog tail ((\(newState, line) -> (newState, [line])) (cmd head state))
prog = undefined

--
-- * Helper Functions
--
