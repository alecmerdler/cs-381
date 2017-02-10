module HW3 (State, cmd, prog) where

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
cmd (Move x2 y2) (state, x1, y1) = ((state, x2, y2), Just ((x1, y1), (x2, y2)))


--
-- | Part II: Implement prog, the semantic function for MiniMiniLogo programs (Prog).
--
-- >>> prog [] (Up, 0, 4)
-- ((Up,0,4),[])
--
-- >>> prog [(Pen Up)] (Down, 0, 0)
-- ((Up,0,0),[])
--
-- >>> prog [(Move 1 1)] (Down, 0, 0)
-- ((Down,1,1),[Just ((0,0),(1,1))])
--
-- >>> prog [(Move 1 2), (Pen Up)] (Down, 0, 0)
-- ((Up,1,2),[Just ((0,0),(1,2))])
--
-- >> prog [(Move 1 2), (Move 1 3)] (Down 0 0)
-- ((Down,1,3),[Just ((0,0),(1,2)),Just ((1,2),(1,3))]
--
prog :: Prog -> State -> (State, [Maybe Line])
prog [] state   = (state, [])
prog cmds state = applyCmds cmds (state, [])


--
-- * Helper Functions
--

--
-- | Recursively applies a list of commands while updating pen state and accummulating drawn lines.
--
-- >>> applyCmds [(Pen Up)] ((Down, 0, 1), [])
-- ((Up,0,1),[])
--
-- >>> applyCmds [(Move 2 2)] ((Down, 0, 0), [])
-- ((Down,2,2),[Just ((0,0),(2,2))])
--
-- >>> applyCmds [(Move 2 2), (Move 3 3)] ((Down, 1, 0), [Just ((0,0),(2,2))])
-- ((Down,3,3),[Just ((2,2),(3,3)),Just ((1,0),(2,2)),Just ((0,0),(2,2))])
--
applyCmds :: [Cmd] -> (State, [Maybe Line]) -> (State, [Maybe Line])
applyCmds [] acc                  = acc
applyCmds (c:cmds) (state, lines) = case line of
                                        Just l -> applyCmds cmds (newState, line : lines)
                                        Nothing -> applyCmds cmds (newState, lines)
                                    where (newState, line) = cmd c state

