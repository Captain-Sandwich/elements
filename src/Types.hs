{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Types
    ( Game(..),
      Card(..),
      Player(..),
      Hand,
      Turn(..),
      GameRep(..),
      Command
    ) where

import Data.MultiSet 
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Aeson
import GHC.Generics

data Player = PlayerOne | PlayerTwo | Neither
        deriving (Show, Ord, Eq, Read)

type Card = Int

type Hand = MultiSet Card

data Game = Game { activePlayer :: Player,
                   hands :: M.Map Player Hand,
                   stack :: [Card],
                   gameOver :: Bool,
                   winner :: Player}
        deriving (Show, Eq)

data Turn = PlayCard Card |
            DiscardSix |
            TakeCard |
            Knock |
            Surrender

instance Show Turn where
        show (PlayCard c) = "Play " ++ show c
        show DiscardSix = "Discard 6"
        show TakeCard = "Take card"
        show Knock = "Knock"
        show Surrender = "Surrender"

data GameRep = GameRep {nOtherHand :: Int,
                        nOtherSixes :: Int,
                        nOwnSixes :: Int,
                        limit :: [Card], 
                        ownHand :: [Card]}
        deriving (Show, Generic, ToJSON, FromJSON)

data Command = Command {cMove :: T.Text}
        deriving (Show, Generic, ToJSON, FromJSON)
