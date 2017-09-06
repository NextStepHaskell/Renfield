module Types.Atomic where

import BasicPrelude

-- | DieFace decribes the possible values of each Die
data DieFace
  = Ghoul
  | Stake
  | HolySymbol
     deriving (Eq,Show)

-- | DieColor describes the possiible colors of each Die.
--   With these values we can distinguish DieFace distribution.
data DieColor
  = Red
  | Green
  | Yellow
     deriving (Eq,Show)

newtype PInt = PInt {fromPInt :: Int}

-- | FaceCount labels the primary purpose for the PInt newtype
type FaceCount = PInt

zero :: FaceCount
zero = toPInt 0

thirteen :: FaceCount
thirteen = toPInt 13

-- | GhoulCount, StakeCount, SymbolCount
-- holds contextualized FaceCounts
newtype GhoulCount  = GhoulCount {fromGhoulCount :: FaceCount}
newtype StakeCount  = StakeCount {fromStakeCount :: FaceCount}
newtype SymbolCount = SymbolCount {fromSymbolCount :: FaceCount}

newtype PlayerName  = PlayerName {fromPlayerName :: Text}

-- | For determining Player Strategy
data PlayerType = Human | TimidBot | BlusterBot  deriving (Eq,Ord,Show)

-- | Values communicating player actions
data Action
  = Roll
  | Pass      -- voluntary end turn
  | Terminate -- game ended your turn
  | NewDice
  | END
     deriving (Eq,Read, Show)

-- | Tracks what stage a player turn is in
data TurnAction = MandatoryRoll | TurnAction | Finished deriving (Eq, Show)

-- | PInt helpers
-- the key difference between a PInt and an Int is
-- truncated subtraction

instance Num PInt where
    x - y = x `truncSub` y
      where
        truncSub (PInt x') (PInt y')
          | y' > x'     = PInt 0
          | otherwise = PInt (x' - y')
          
    x + y = PInt (fromPInt x + fromPInt y)
    
    x * y = PInt (fromPInt x * fromPInt y)

    abs x = x

    signum _ = 1

    fromInteger x = PInt (fromInteger x)

instance Eq PInt where
    x == y = fromPInt x == fromPInt y
    x /= y = fromPInt x /= fromPInt y

instance Ord PInt where
    x <= y = fromPInt x <= fromPInt y
    x < y  = fromPInt x < fromPInt y
    x > y  = fromPInt x > fromPInt y

instance Show PInt where
    show = show . fromPInt

toPInt :: Int -> PInt
toPInt x
  | x <= 0    = 0
  | otherwise = PInt x
