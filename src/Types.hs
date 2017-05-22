{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Types
    ( Game(..),
      Card(..),
      Player(..),
      Hand,
      Turn(..),
      GameRep(..),
      Command(..),
    ) where

import Data.MultiSet 
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Monoid
import Control.Applicative ((<|>))
import Data.Aeson
import GHC.Generics

data Player = PlayerOne | PlayerTwo | Neither
        deriving (Show, Ord, Eq, Read)

type Card = Int

type Hand = MultiSet Card

data Game = Game { activePlayer :: Player,
                   hands :: M.Map Player Hand,
                   sixes :: M.Map Player Int,
                   stack :: [Card],
                   gameOver :: Bool,
                   winner :: Player}
        deriving (Show, Eq)

data Turn = PlayCard Card |
            DiscardSix |
            TakeCard |
            Knock |
            Surrender
        deriving (Generic, ToJSON, FromJSON)

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

data Command = Command {origin :: T.Text, move :: Turn}
        deriving (Show, Generic, ToJSON, FromJSON)
