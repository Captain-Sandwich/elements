{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Elements
    ( newGame,
      parseTurn,
      shuffleDeck,
      applyValidTurn,
      turnLogic,
      Game(..),
      Player(..),
      renderGame
    ) where

import Types
import System.Random
import System.Random.Shuffle (shuffle')
import qualified Data.MultiSet as MS
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Control.Monad.State
import Data.List (intersperse)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Attoparsec.Text as AP
import qualified Data.Text as T
import Control.Applicative ((<|>))

-- game setup
rng = unsafePerformIO $ getStdGen

deck :: [Card]
deck = [1,1,2,2,3,3,4,4,5,5,6,6,6,6,6,6]

shuffleDeck :: RandomGen gen => gen -> [Card]
shuffleDeck g = shuffle' deck (length deck) g

newGame :: [Card] -> Game
newGame deck = Game PlayerOne hs sixes [] False Neither
        where h1 = MS.fromList $ take 6 deck
              h2 = MS.fromList $ take 6 $ drop 6 deck
              hs = M.fromList $ [(PlayerOne, h1), (PlayerTwo, h2)]
              sixes = M.fromList $ [(PlayerOne, 0), (PlayerTwo, 0)]

game = newGame $ shuffleDeck rng

-- helpers
otherPlayer p = if p == PlayerOne then PlayerTwo else PlayerOne

nextPlayer :: Game -> Game
nextPlayer g@Game{..} = g {activePlayer = otherPlayer activePlayer}

handSum :: Game -> Player -> Int
handSum g p = sum $ MS.toList $ getPlayerHand g p

getPlayerHand :: Game -> Player -> Hand
getPlayerHand Game{..} p = fromJust $ M.lookup p hands

applyToHand :: Game -> Player -> (Hand -> Hand) -> Game
applyToHand g@Game{..} p f = g {hands = hands'}
        where hands' = M.adjust f p hands

addToHand :: Game -> Player -> Card -> Game
addToHand g p c = applyToHand g p (MS.insert c)

removeFromHand :: Game -> Player -> Card -> Game
removeFromHand g p c = applyToHand g p (MS.delete c)


-- game logic
performTurn :: Game -> Turn -> Game
performTurn g t = if moveIsValid g t then turnLogic g t else error "invalid turn"

applyValidTurn :: Game -> Turn -> Game
applyValidTurn g t = if moveIsValid g t then turnLogic g t else g

turnLogic :: Game -> Turn -> Game
turnLogic g@Game{..} (PlayCard c) = nextPlayer $ removeFromHand (g {stack=c:stack}) activePlayer c
turnLogic g@Game{..} (DiscardSix) = nextPlayer $ game' {sixes = M.adjust ((+) 1) activePlayer sixes}
        where game' = removeFromHand g activePlayer 6
turnLogic g@Game{..} (TakeCard) = nextPlayer $ addToHand (g {stack=stack'}) activePlayer topCard
              where (topCard:stack') = stack
turnLogic g@Game{..} (Knock) = g {gameOver = True, winner = winner'}
        where otherSum = handSum g $ otherPlayer activePlayer
              ownSum = handSum g activePlayer
              winner' = if otherSum > limit then activePlayer else
                        if ownSum > otherSum then activePlayer else otherPlayer activePlayer
              limit = sum stack
turnLogic g@Game{..} (Surrender) = g {gameOver = True, winner = otherPlayer activePlayer}


moveIsValid :: Game -> Turn -> Bool
moveIsValid (Game _ _ _ _ True _)  _ = False
moveIsValid g@Game{..} (PlayCard c) = MS.member c $ getPlayerHand g activePlayer
moveIsValid g@Game{..} DiscardSix = MS.member 6 $ getPlayerHand g activePlayer
moveIsValid g@Game{..} TakeCard = not $  (==) [] stack
moveIsValid g@Game{..} Knock = (sum stack) >= (handSum g activePlayer)
moveIsValid Game{..} (Surrender) = True

listMoves :: Game -> [Turn]
listMoves g = playcards ++ rest
        where playcards = (map (PlayCard . fst) $ MS.toOccurList $ getPlayerHand g $ activePlayer g)
              rest = filter (moveIsValid g) [DiscardSix, TakeCard, Knock, Surrender]

inputTurn :: Game -> IO Turn
inputTurn g = do
        i <- T.pack <$> getLine
        case AP.parseOnly parseTurn i of
                (Right t) -> do
                                print t
                                if moveIsValid g t
                                        then return t
                                        else inputTurn g 
                (Left _) -> do
                                print "Invalid move"
                                inputTurn g


showGame g@Game{..} Neither = concat $ intersperse "\n\n" $ (show activePlayer):lines
        where lines = map (intersperse ' ' . show) [hand1, reverse stack, hand2]
              hand1 = MS.toList $ getPlayerHand g PlayerOne
              hand2 = MS.toList $ getPlayerHand g PlayerTwo

showGame g@Game{..} p = concat $ intersperse "\n\n" $ (show activePlayer):lines
        where lines = map (intersperse ' ') [hand1, stack', hand2]
              hand1 = show $ replicate n1 'X'
              n1 = MS.size $ getPlayerHand g $ otherPlayer p
              hand2 = show $ MS.toList $ getPlayerHand g p
              stack' = show $ reverse stack

renderGame :: Game -> Player -> GameRep
renderGame g@Game{..} p = GameRep {nOtherSixes = M.findWithDefault 0 p' sixes,
                                   nOtherHand = MS.size $ getPlayerHand g p',
                                   nOwnSixes = M.findWithDefault 0 p' sixes,
                                   ownHand = MS.toList $ getPlayerHand g p,
                                   limit = stack}
        where p' = otherPlayer p


loop :: Game -> IO Player
loop g = do
        putStrLn $ showGame g Neither
        print "Current Player:"
        print $ activePlayer g
        print "What is your Turn"
        t <- inputTurn g
        let g' = turnLogic g t
        if gameOver g' then do
                print "Winner:"
                return $ winner g'
        else
                loop g'


-- parsers
allSubStrings :: T.Text -> [T.Text]
allSubStrings s = map (flip T.take s) $ reverse [1..n]
        where n = T.length s

shortStringParsers :: T.Text -> AP.Parser T.Text
shortStringParsers = AP.choice . map AP.string . allSubStrings

parseTurn :: AP.Parser Turn
parseTurn = parsePlayCard <|>
            (shortStringParsers "discard" >> return DiscardSix) <|>
            (shortStringParsers "knock" >> return Knock) <|>
            (shortStringParsers "take" >> return TakeCard) <|>
            (shortStringParsers "surrender" >> return Surrender)

parsePlayCard :: AP.Parser Turn
parsePlayCard = do
        shortStringParsers "play"
        AP.char ' '
        x <- AP.decimal
        return $ PlayCard x

