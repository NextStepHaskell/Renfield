{-# LANGUAGE OverloadedStrings #-}
module Types.Composite.Message where

import BasicPrelude

import Types.Atomic
import Types.Composite.Dice (SideDice)

data Message
  = GotStaked
  | StakeRound StakeCount
  | GhoulsRound GhoulCount
  | HolySymbols SymbolCount [DieColor]
  | FinalTurn PlayerName
  | AllTheGhouls
  | ReturnDice
  | NewTurn PlayerName
  | ReportM Report
  | MandatoryRollMsg PlayerName
  | TurnActionMsg PlayerName
  | FinishedMsg PlayerName
    deriving Show

data Report = Report
  { _current_player_rpt :: PlayerName
  , _side_dice_rpt      :: SideDice
  , _ghoul_count_rpt    :: GhoulCount
  , _scores_rpt         :: [(PlayerName,GhoulCount)]
  , _stake_count_rpt    :: StakeCount
  , _other              :: [Message]
  } deriving Show

mkReport :: PlayerName                ->
              SideDice                  ->
              GhoulCount                ->
              [(PlayerName,GhoulCount)] ->
              StakeCount                ->
              [Message]                 ->
              Report
mkReport p_name s_dice g_count score s_count msg =
  Report p_name s_dice g_count score s_count msg

ppMessage :: Message -> Text
ppMessage GotStaked     = "Van Helsing staked you!\n"
ppMessage (StakeRound sc) = "You've rolled " <> (tshow sc) <> " stakes.\n"
ppMessage (GhoulsRound gc) = "You've rolled " <> (tshow gc) <> " ghouls.\n"
ppMessage (HolySymbols sc dcolors) = "You've rolled " <> 
                                     (tshow sc)       <>
                                     " side dice: "   <>
                                     (unwords $ map tshow dcolors) <> ".\n"
ppMessage (FinalTurn p_name) = "Final Turn " <> (tshow p_name) <> ".\n"
ppMessage AllTheGhouls       = "You captured them all.\n"
ppMessage ReturnDice         = "More dice for you!\n"
ppMessage (NewTurn p_name)   = (tshow p_name) <> " begins a new turn!\n"
ppMessage (ReportM rpt)      = (tshow rpt) <> "\n"
ppMessage (MandatoryRollMsg p_name) = "Time To Roll, " <> (tshow p_name) <>"\n"
ppMessage (TurnActionMsg p_name)    = "Okay what's the next move, " <> 
                                      (tshow p_name)                <>
                                      "\n"
ppMessage (FinishedMsg p_name) = (tshow p_name) <> " has finished their turn.\n" 
                                   

