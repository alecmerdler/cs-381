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
-- >>> test (Facing North) (\x -> Nothing) ((1,1), North, 0)
-- True
--
test :: Test -> World -> Robot -> Bool
test (Not t) _ _ = undefined
test (Facing c1) _ (_, c2, _) = c1 == c2
test (Clear d) _ _ = undefined
test (Beeper) _ _ = undefined
test (Empty) _ _ = undefined


-- | Valuation function for Stmt.
--
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
stmt _ _ _ _ = undefined


-- | Run a Karel program.
--
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
