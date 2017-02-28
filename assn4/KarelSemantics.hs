--
-- CS381
-- Assignment #4
-- Alec Merdler
--

module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)
import KarelSyntax
import KarelState


-- | Valuation function for Test.
--
-- >>> test (Facing North) (\x -> Nothing) ((1, 1), North, 0)
-- True
--
-- >>> test (Facing West) (\x -> Nothing) ((1, 1), North, 0)
-- False
--
-- >>> test (Clear Front) (\x -> Nothing)  ((1, 1), North, 0)
-- False
--
-- >>> test (Clear Front) (\x -> Just 0)  ((1, 1), North, 0)
-- True
--
-- >>> test Beeper (\x -> Nothing)  ((1, 1), North, 0)
-- False
--
-- >>> test Beeper (\x -> Just 1)  ((1, 1), North, 0)
-- True
--
-- >>> test Empty (\x -> Nothing) ((1, 1), North, 0)
-- True
--
-- >>> test Empty (\x -> Nothing) ((1, 1), North, 2)
-- False
--
test :: Test -> World -> Robot -> Bool
test (Not t) w r     = not (test t w r)
test (Facing c1) _ r = c1 == getFacing r
test (Clear d) w r   = isClear (relativePos d r) w
test (Beeper) w r    = hasBeeper (getPos r) w
test (Empty) _ r     = isEmpty r


-- | Valuation function for Stmt.
--
-- >>> stmt Shutdown [] (\x -> Nothing) ((1, 1), North, 0)
-- Done: ((1,1),North,0)
--
-- >>> stmt PickBeeper [] (\x -> Nothing) ((1, 1), North, 0)
-- Error: No beeper to pick at: (1,1)
--
-- >>> stmt PickBeeper [] (\x -> Just 1) ((1, 1), North, 0)
-- OK: ((1,1),North,1)
--
-- >>> stmt PutBeeper [] (\x -> Nothing) ((1, 1), North, 0)
-- Error: No beepers in bag
--
-- >>> stmt PutBeeper [] (\x -> Nothing) ((1, 1), North, 2)
-- OK: ((1,1),North,1)
--
-- >>> stmt (Turn Left) [] (\x -> Nothing) ((1, 1), North, 0)
-- OK: ((1,1),West,0)
--
-- >>> stmt (Block []) [] (\x -> Nothing) ((1, 1), North, 0)
-- OK: ((1,1),North,0)
--
-- >>> stmt (Block [PickBeeper, Shutdown]) [] (\x -> Just 1) ((1, 1), North, 0)
-- Done: ((1,1),North,1)
--
-- >>> stmt (Block [PickBeeper, Shutdown]) [] (\x -> Nothing) ((1, 1), North, 0)
-- Error: No beeper to pick at: (1,1)
--
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown _ _ r       = Done r
stmt PickBeeper _ w r     = let p = getPos r
                            in if hasBeeper p w
                                  then OK (decBeeper p w) (incBag r)
                                  else Error ("No beeper to pick at: " ++ show p)
stmt PutBeeper _ w r      = let p = getPos r
                            in if isEmpty r
                                  then Error ("No beepers in bag")
                                  else OK (incBeeper p w) (decBag r)
stmt (Turn d) _ w r       = OK w (setFacing (cardTurn d (getFacing r)) r)
stmt (Block []) _ w r     = OK w r
stmt (Block (s:ss)) d w r = case stmt s d w r of
                                 (OK w' r') -> stmt (Block ss) d w' r'
                                 (Done r')  -> Done r'
                                 (Error e)  -> Error e
stmt _ _ _ _ = undefined


-- | Run a Karel program.
--
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
