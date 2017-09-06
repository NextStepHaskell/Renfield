module Types.Composite.Dice where

import Data.Sized (Sized)
import Data.Type.Natural (Three,Six,Thirteen)
import System.Random (StdGen)

import Types.Atomic

type DieFaces = Sized [] Six DieFace
type DiceInPlay = Sized [] Three Die
type RollResult = Sized [] Three (DieColor, DieFace)
type Cup        = Sized [] Thirteen Die

type Die       = (DieColor,DieFaces)
type CIP       = [Die]
type Remainder = [Die]
type SideDice  = [Die]
type DieResult = (DieColor,DieFace)

data RollGenerators = RollGenerators StdGen StdGen StdGen
newtype CupGenerator = CupGenerator StdGen
