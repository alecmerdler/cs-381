--
-- Practice problems for Midterm 2
-- CS381
--

module Midterm2Review where

import Prelude

--
-- | 1. Consider the following abstract for a language for describing times. Midnight and Noon represent constant times,
--      AM and PM can be used to represent times on the hour in the morning or afternoon/evening, respectively, and
--      Before and After can be used to represent a time that is a given number of minutes before or after another time.
--

type Hour = Int
type Minutes = Int

data Time = Midnight
          | Noon
          | AM Hour
          | PM Hour
          | Before Minutes Time
          | After Minutes Time
  deriving (Eq,Show)


-- | (a) Implement a denotational semantics for this language using Int as the semantic domain, where the integer
--       represents the number of minutes since midnight. For example, the time 8:13am could be represented by the
--       expression After 13 (AM 8), and would be mapped to the semantic value 493. For this version of the semantics,
--       you may assume that all hour values are between 1 and 12. It is OK for the resulting semantic value to be
--       negative or a number larger than the number of minutes in a 24-hour day.
--
time :: Time -> Int
time Midnight     = 0
time Noon         = 60 * 12
time (AM h)       = h * 60
time (PM h)       = (time Noon) + (h * 60)
time (Before m t) = (time t) - m
time (After m t)  = (time t) + m


-- | (b) Implement a revised version of this denotational semantics that checks to make sure that all hour values are
--       between 1 and 12, and returns an error otherwise.
--
-- >>> time' (AM 13)
-- Nothing
--
-- >>> time' (AM 2)
-- Just 120
--
time' :: Time -> Maybe Int
time' Midnight     = Just 0
time' Noon         = Just (60 * 12)
time' (AM h)       = if (h > 1 && h < 13)
                        then Just (h * 60)
                        else Nothing
time' (PM h)       = undefined
time' (Before m t) = undefined
time' (After m t)  = undefined


--
-- | 2. Consider the following abstract syntax for a language describing movements on a 2-dimensional plane. The JumpTo
--      construct immediately moves to the given position. The GoUp construct moves the current position vertically the
--      indicated number of steps (a negative value will move the current position down). The GoRight construct moves
--      the current position the indicated number of steps horizontally (negative = left). The Seq construct performs
--      the left move followed by the right move. Define a denotational semantics for this language.
--

type Pos = (Int,Int)

data Move = JumpTo Pos
          | GoUp Int
          | GoRight Int
          | Seq Move Move
  deriving (Eq,Show)


--
-- | 3. Consider the following abstract syntax for a language for building and manipulating non-nested integer lists.
--      Your task is to implement a static type system for this language. Note that the language does *not* support
--      nested lists. That is, there are only two valid types in our language: lists and integers, anything else is a
--      type error.
--

data Expr = N Int
          | Empty
          | Cons Expr Expr
          | Head Expr
          | Tail Expr
  deriving (Eq,Show)

data Type = TInt | TList | Error
