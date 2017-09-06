{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

module Types.Composite.GameState where

import BasicPrelude

import Control.Monad.State
import Control.Monad.Prompt
import Control.Monad.Reader

import Types.Atomic
import Types.Composite.Dice
import Types.Composite.Message

type GameT a        = ReaderT GameConfig (StateT GameS IO) a
newtype GameConfig  = GameConfig {toStrategyMap :: StrategyMap}
newtype StrategyMap = StrategyMap (Map PlayerType Strategy)

type GameP a = Prompt RenfieldP a
type PlayerMap = Map PlayerName Player

type TurnOrder = [PlayerName]


type RollEval = (Bool,GhoulCount) -> StakeResult -> Maybe TurnResult

type GameLensFunctor a = forall f. Functor f => (a -> f a) -> GameS -> f GameS
type GameLensApplicative a = 
  forall f. Applicative f => (a -> f a) -> GameS -> f GameS

-- | The RenField DSL
data RenfieldP a where
  GetPlayerName :: RenfieldP PlayerName
  GetTurnAction :: RenfieldP TurnAction
  GetReport     :: RenfieldP Report
  CheckWinner   :: RenfieldP Winner
  Say           :: Message    -> RenfieldP ()
  GetAction     :: TurnAction -> RenfieldP Action
  EvalTurn      :: NextPlayer -> ActionResult -> RenfieldP Iteration
  EvalAction    :: RollEval   -> Action       -> RenfieldP ActionResult

data GameS = GameS
  { _game_data_gs    :: GameData
  , _player_turn_gs  :: PlayerTurn
  , _turn_tracker_gs :: TurnTracker
  }

data GameData = GameData
  { _cup_in_play_gd    :: CIP
  , _players_gd        :: PlayerMap
  , _report_gd         :: Report
  }

data PlayerTurn = PlayerTurn
  { _side_dice_pd   :: SideDice
  , _ghoul_count_pd :: GhoulCount
  , _stake_count_pd :: StakeCount
  } deriving Show

init_PlayerTurn :: PlayerTurn
init_PlayerTurn = PlayerTurn
  { _side_dice_pd = []
  , _ghoul_count_pd = zero
  , _stake_count_pd = zero
  }

data TurnTracker = TurnTracker
  { _turn_order_tt  :: TurnOrder
  , _turn_action_tt :: TurnAction
  }

data RollTotals = RollTotals
  { ghoul_count_rt :: GhoulCount
  , stake_count_rt :: StakeCount
  , side_dice_rt   :: SideDice
  } deriving Show

data Player = Player
  { _p_type      :: PlayerType
  , _score       :: GhoulCount
  }

data PRR = PRR
  { ghoul_count_prr'    :: GhoulCount
  , hsymbol_count_prr'  :: (SymbolCount,[DieResult])
  , stake_count_prr'    :: StakeCount
  , remaining_dice_prr' :: Remainder
  } deriving Show

data Iteration
  = Abort PlayerName
  | Completed Winner
  | ContinuePlay
    deriving (Show,Ord,Eq)

data StakeResult
  = Staked_
  | NotStaked StakeCount
    deriving (Eq,Show)

data TurnResult -- The result of a normal game turn
  = Ghouled GhoulCount -- means you've gotten your thirteen ghouls
  | Staked  -- Van Helsing staked you
  | Other (Remainder,RollTotals) -- Something else happened
     deriving Show

data ActionResult
  = RollAction TurnResult
  | PassAction GhoulCount
  | TermAction
  | NewDiceAction CIP
  | EndAction
  | NoopAction

data Winner
  = Winner (PlayerName,GhoulCount)
  | Winners [(PlayerName,GhoulCount)]
     deriving (Eq,Ord,Show)

type NextPlayer = GameS -> GameS

type Strategy = GameS -> IO Action
type ScoreMap = Map PlayerName GhoulCount
