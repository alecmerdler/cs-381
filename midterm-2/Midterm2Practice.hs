-- 
-- * Time Language
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

noon :: Int
noon = 12 * 60

time :: Time -> Int
time Midnight     = 0
time Noon         = noon
time (AM h)       = h * 60
time (PM 12)      = noon
time (PM h)       = noon + h * 60
time (Before m t) = time t - m
time (After m t)  = time t + m

checkHour :: Hour -> Bool
checkHour h = h >= 0 && h <= 12

time' :: Time -> Maybe Int
time' Midnight     = Just 0
time' Noon         = Just noon
time' (AM h)       = if checkHour h then Just (h * 60) else Nothing
time' (PM 12)      = Just noon
time' (PM h)       = if checkHour h then Just (noon + h * 60) else Nothing
time' (Before m t) = case time' t of
                       Just i -> Just (i - m)
                       Nothing -> Nothing
time' (After m t)  = case time' t of
                       Just i -> Just (i + m)
                       Nothing -> Nothing


--
-- * Move Language
--

type Pos = (Int,Int)

data Move = JumpTo Pos
          | GoUp Int
          | GoRight Int
          | Seq Move Move
  deriving (Eq,Show)

move :: Move -> Pos -> Pos
move (JumpTo p)  _     = p
move (GoUp i)    (x,y) = (x, y+i)
move (GoRight i) (x,y) = (x+i, y)
move (Seq m1 m2) p     = move m2 (move m1 p)


--
-- * List Language
--

data Expr = N Int
          | Empty
          | Cons Expr Expr
          | Head Expr
          | Tail Expr
  deriving (Eq,Show)

data Type = TInt | TList | Error

typeOf :: Expr -> Type
typeOf (N _)      = TInt
typeOf Empty      = TList
typeOf (Cons h t) = case (typeOf h, typeOf t) of
                      (TInt, TList) -> TList
                      _ -> Error
typeOf (Head e)   = case typeOf e of
                      TList -> TInt
                      _ -> Error
typeOf (Tail e)   = case typeOf e of
                      TList -> TList
                      _ -> Error
