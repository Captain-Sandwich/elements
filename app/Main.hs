{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import qualified Data.Bimap as BM
import qualified Data.Attoparsec.Text as AP
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Base64.Lazy as BSL
import qualified Data.ByteString as BS
import Network.HTTP.Types.Status (badRequest400)
import Data.Aeson (decode, encode)
import Web.Spock
import Web.Spock.Config
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
import Control.Monad.Trans
import Control.Lens hiding (element)
import Data.Monoid
import Data.Maybe
import Data.Char (chr)
import Data.Time.Clock
import Data.IORef
import System.Random

import Elements
import Types

configPath = "/home/chicken/.elements.cfg"

data GameSession = GameSession {gameState :: Game, players :: BM.Bimap PlayerID Player} -- maps players to telegram (or client) ids
    deriving (Show)

data ElementsState = ElementsState {games :: M.Map Int GameSession, ngames :: Int}
    deriving (Show)

data MySession = EmptySession
data MyAppState = DummyAppState (IORef ElementsState)
type PlayerID = T.Text


rng = unsafePerformIO $ getStdGen

main :: IO ()
main = do ref <- newIORef $ ElementsState M.empty 0
          spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
          runSpock 61748 (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app =
    do
       middleware $ logStdoutDev
       middleware $ staticPolicy (addBase "static")
       get root $
           text "Hello World!"
--       get ("hello" <//> var) $ \name ->
--           do (DummyAppState ref) <- getState
--              visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
--              text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))
       get ("game/new") $ 
           do (DummyAppState ref) <- getState
              newGameID <- liftIO $ atomicModifyIORef' ref $ addNewGame rng "dummyID"--TODO: insert player id from request
              redirect $ "/game/" <> (T.pack $ show newGameID)
       get ("game" <//> var) $ \gameID ->
           do (DummyAppState ref) <- getState
              state <- liftIO $ readIORef ref
              case M.member gameID $ games state of
                        True -> do
                            pid <- getOrSetIdentifier
                            --json pid
                            let (Just session) = M.lookup gameID $ games state
                            case BM.ismember pid $ players session
                                True -> 
                                False -> redirect "/"
                            --redirect "/"
                        False -> redirect "/"
       post ("game" <//> var) $ \gameID ->
            do (DummyAppState ref) <- getState
               b <- body
               liftIO $ BS.putStrLn b
               let c = decode $ BSL.fromStrict b :: Maybe Command
               liftIO $ print c
               let d = gameID :: Int
               liftIO $ print gameID
               case c of
                    Just command -> do
                        response <- liftIO $ atomicModifyIORef' ref $ processCommand gameID command
                        json response
                    Nothing -> do
                        setStatus badRequest400

getOrSetIdentifier = do
    pid <- cookie "pid"
    case pid of
        (Just x) -> return x
        --Nothing -> return "testor"
        Nothing -> do
            let pid' = T.pack $ take 32 $ randomRs ('a', 'z') $ unsafePerformIO newStdGen
            --setCookie "pid" pid' (fromInteger (3600*48))
            setCookie "pid" pid' defaultCookieSettings
            return pid'

addNewGame :: StdGen -> PlayerID -> ElementsState -> (ElementsState, Int)
addNewGame rng pid state = (state', newID)
        where game = newGame $ shuffleDeck rng
              newID = (+1) $ ngames state
              rand = fst $ random rng :: Int
              playerPosition = if even rand then PlayerOne else PlayerTwo
              newSession = GameSession game $ BM.singleton pid playerPosition
              state' = state {ngames = newID, games = games state <> M.singleton newID newSession} :: ElementsState

processCommand :: Int -> Command -> ElementsState -> (ElementsState, GameRep)
processCommand gameID c state = (state {games = sessions'}, rep)
    where (Just session) = M.lookup gameID $ games state
          game = gameState session
          turn = move c
          game' = applyValidTurn game turn
          session' =  if hasTwoPlayers session then session {gameState = game'} else session
          sessions' = M.insert gameID session' $ games state
          player = PlayerOne
          rep = renderGame game' player

hasTwoPlayers :: GameSession -> Bool
hasTwoPlayers = ((==) 2) . BM.size . players