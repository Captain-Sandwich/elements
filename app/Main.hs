{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where


--import           Network.HTTP.Client      (newManager, Manager, httpLbs, withResponse, parseRequest)
--import           Network.HTTP.Client.TLS  (tlsManagerSettings)
--import           Web.Telegram.API.Bot
--import           Network.CGI
--import           Network.FastCGI (runFastCGIorCGI)
--import           Data.Time (getCurrentTime)
--import           Data.Time.Format (formatTime, defaultTimeLocale)
--import           Text.Printf (printf)
--import           Data.Aeson
import           System.IO.Unsafe (unsafePerformIO)
--import           Data.Monoid
--import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
--import qualified Data.Text.Lazy as TL
--import qualified Data.Text.Lazy.Builder as TB
--import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Map as M
import qualified Data.Bimap as BM
import Web.Spock
import Web.Spock.Config
import Control.Monad.Trans
import Data.Monoid
import Data.IORef
import System.Random (getStdGen)

import Elements
import Types

configPath = "/home/chicken/.elements.cfg"

data Session = Session {sessionID :: Int,
                        gameState :: Game,
                        players :: BM.Bimap Player T.Text} -- maps players to telegram (or client) ids

data ElementsState = ElementsState {games :: M.Map Int Game, ngames :: Int}
data MySession = EmptySession
data MyAppState = DummyAppState (IORef ElementsState)


rng = unsafePerformIO $ getStdGen

main :: IO ()
main = do ref <- newIORef $ ElementsState M.empty 0
          spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
          runSpock 61748 (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app =
    do get root $
           text "Hello World!"
--       get ("hello" <//> var) $ \name ->
--           do (DummyAppState ref) <- getState
--              visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
--              text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))
       get ("game/new") $ 
           do (DummyAppState ref) <- getState
              newGameID <- liftIO $ atomicModifyIORef' ref addNewGame
              redirect $ "/game/" <> (T.pack $ show newGameID)
       get ("game" <//> var) $ \gameID ->
           do (DummyAppState ref) <- getState
              state <- liftIO $ readIORef ref
              case M.member gameID $ games state of
                        True -> text "Game exists, hurray"
                        False -> text "Game does not exist"
       post ("game" <//> var) $ \command ->
        do (DummyAppState ref) <- getState
           response <- liftIO $ atomicModifyIORef' ref $ processCommand command
           json response



addNewGame :: ElementsState -> (ElementsState, Int)
addNewGame state = (state', newID)
        where game = newGame $ shuffleDeck rng
              newID = (+1) $ ngames state
              state' = state {ngames = newID, games = games state <> M.singleton newID game} :: ElementsState

processCommand :: Command -> ElementsState -> (ElementsState, GameRep)
processCommand = undefined


--main :: IO ()
--main = do
--  (Just config) <- decode <$> BS.readFile configPath :: IO (Maybe Config)
--  manager <- newManager tlsManagerSettings
--  res <- getMe (token config) manager
--  case res of
--    Left e -> do
--      print e
--    Right Response { result = u } -> do
--      print $ "My name is " <> user_first_name u
--  updateLoop config manager 1
--
--
---- bot main loop
--updateLoop :: Config -> Manager -> Int -> IO ()
--updateLoop config@Config{..} manager offset = do
--        res <- getUpdates token (Just offset) (Just 1) (Just timeout) manager
--        case res of
--                Left e -> do
--                        updateLoop config manager offset
--                Right Response { result = [Update {update_id = last_offset, message = msg}]} -> do
--                        putStrLn $ show last_offset
--                        case msg of
--                                Just m -> dispatchMessage config manager m
--                                Nothing -> return ()
--                        updateLoop config manager (last_offset + 1)
--
---- main dispatcher
--dispatchMessage :: Config -> Manager -> Message -> IO ()
--dispatchMessage config@Config{..} manager Message { text = Just body, chat = Chat {chat_id = chatid} } = do
--        print "received message:"
--        print body
--        case T.stripPrefix "/" body of
--                Nothing -> return ()
--                Just x -> do
--                        print "Command Detected:"
--                        let args = T.splitOn " " x
--                        let cmd = head args
--                        let action = dispatchLookup $ T.toLower cmd
--                        print cmd
--                        response <- action config args
--                        case response of
--                                Nothing -> return ()
--                                Just t -> do
--                                        let cid = TL.toStrict $ TB.toLazyText $ TB.decimal chatid
--                                        let req = sendMessageRequest cid t
--                                        sendMessage token req manager
--                                        return ()
----ignore all other message types
--dispatchMessage _ _ _ = do
--                        print "Ignoring invalid messages"
--                        return ()
--
--dispatchTable :: M.Map T.Text (Config -> [T.Text] -> IO (Maybe T.Text))
--dispatchTable = M.fromAscList [("log", logAction), ("yt", YT.youtubeAction)]
--
--dispatchLookup x = M.findWithDefault emptyAction x dispatchTable
--
--emptyAction :: Config -> [T.Text] -> IO (Maybe T.Text)
--emptyAction _ (x:_) = do
--        return $ Just $ "no action for \"" <> x <> "\"."
--
---- weight log part
--logAction :: Config -> [T.Text] -> IO (Maybe T.Text)
--logAction _ (_:a:_) = do
--                case double a of
--                        Right (x,_) -> do
--                                        addWeight x
--                                        return $ Just "added data point"
--                        _ -> return Nothing
--
--
--addCsv :: FilePath -> String -> Double -> IO ()
--addCsv file formatString x = do
--        datestring <- formatTime defaultTimeLocale "%Y-%m-%d" <$> getCurrentTime
--        appendFile file $ printf formatString datestring x
--
--addWeight :: Double -> IO ()
--addWeight = addCsv  "/home/chicken/html/gewichtlog.tsv" "%s\t\t%.1f\n"
--
