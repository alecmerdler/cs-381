module HW3 (cmd) where

import Render
import MiniMiniLogo


-- | Represents the state of the pen
type State = (Mode, Int, Int)


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
-- ((Up,0,4),[Nothing])
--
-- >>> prog [(Pen Up)] (Down, 0, 0)
-- ((Up,0,0),[Nothing])
--
-- >>> prog [(Move 1 1)] (Down, 0, 0)
-- ((Down,1,1),[Just ((0,0),(1,1))])
--
-- >> prog [(Move 1 2), (Pen Up)] (Down, 0, 0)
-- ((Up,1,2),[Just ((0,0),(1,2))])
--
-- >> prog [(Move 1 2), (Move 1 3)] (Down 0 0)
-- ((Down,1,3),[Just ((0,0),(1,2)),Just ((1,2),(1,3))]
--
prog :: Prog -> State -> (State, [Maybe Line])
prog [] state       = (state, [Nothing])
prog (c:cmds) state = applyCmd c (state, [])


--
-- * Helper Functions
--

--
-- | Applies a given command to an existing state and set of lines
--
-- >>> applyCmd (Pen Down) ((Up, 0, 0), [])
-- ((Down,0,0),[Nothing])
--
-- >>> applyCmd (Move 1 1) ((Down, 0, 0), [Just ((0, 0), (2, 2))])
-- ((Down,1,1),[Just ((0,0),(1,1)),Just ((0,0),(2,2))])
--
applyCmd :: Cmd -> (State, [Maybe Line]) -> (State, [Maybe Line])
applyCmd newCmd (state, [])    = (\ (newState, line) -> (newState, [line])) (cmd newCmd state)
applyCmd newCmd (state, lines) = (\ (newState, line) -> (newState, line : lines)) (cmd newCmd state)
