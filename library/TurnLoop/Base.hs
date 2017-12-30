module TurnLoop.Base where

import qualified Data.Map as Map
import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.MVar ()
import Control.Concurrent.Chan ()
import Data.Map (Map)

type Many rep a = (rep -> a)

data Step state terminal = Step
  { sState :: state
  , sTerminal  :: Maybe terminal
  } deriving (Show, Eq)

data Result sessionId rep userId state extra = Result
  { rStarter :: Starter sessionId rep userId
  , rState :: state
  , rExtra :: extra
  }

data Starter sessionId rep userId = Starter
  { sSessionId :: sessionId
  , sUserIds :: Many rep userId
  }

data Registry userId user m = Registry
  { rInsertUser :: user -> m userId
  , rGetUserById :: userId -> m (Maybe user)
  }

data Lobby sessionId rep userId m = Lobby
  { lTransferUser :: userId -> m (Maybe (Starter sessionId rep userId))
  , lDequeueUser :: sessionId -> m (Maybe userId)
  , lAnnounceSession :: Starter sessionId rep userId -> m ()
  }

data SessionEntry rep input state terminal m = SessionEntry
  { seThread :: Thread m
  , seGames :: Many rep (Session input state terminal)
  }

data Session input state terminal = Session
  { sInput :: Chan input
  , sStep :: Chan (Step state terminal)
  }

data SessionRecord userId rep input state terminal m = SessionRecord
  { srThread :: Thread m
  , srLabeled :: Many rep (LabeledSession userId input state terminal)
  }

data LabeledSession userId input state terminal = LabeledSession
  { lsUserId :: userId
  , lsSession :: Session input state terminal
  }

data Sessions sessionId userId rep input state terminal m = Sessions
  { sInsertSession :: (sessionId, SessionRecord userId rep input state terminal m) -> m ()
  , sFindSession :: sessionId -> m (Maybe (SessionRecord userId rep input state terminal m))
  , sRemoveSession :: sessionId -> m ()
  }

data Results sessionId rep userId state extra m = Results
  { rSaveResult :: Result sessionId rep userId state extra -> m ()
  , rFindResult :: sessionId -> m (Maybe (Result sessionId rep userId state extra))
  }

data Thread m = Thread
  { tThreadId :: ThreadId
  , tKill :: m ()
  }

-- Session

newSession :: MonadIO m => m (Session input state terminal)
newSession = liftIO $ Session <$> newChan <*> newChan

-- Lobby

type UserQueue sessionId rep userId = [(userId, MVar (Starter sessionId rep userId))]
type AnnounceDeck sessionId rep userId = Map sessionId [MVar (Starter sessionId rep userId)]

newLobbyFIFO :: (MonadIO m, Ord sessionId) => m (Lobby sessionId rep userId m)
newLobbyFIFO = liftIO $ do
  p <- liftIO $ newTVarIO []
  w <- liftIO $ newTVarIO Map.empty
  return Lobby
    { lTransferUser = transferUser p
    , lDequeueUser = dequeueUser p w
    , lAnnounceSession = announceSession w }
  where
    transferUser
      :: MonadIO m
      => TVar (UserQueue sessionId rep userId)
      -> userId
      -> m (Maybe (Starter sessionId rep userId))
    transferUser p i = liftIO $ do
      ref <- newEmptyMVar
      atomically $ modifyTVar p $ \q -> q ++ [(i, ref)]
      starter <- takeMVar ref
      return (Just starter)

    dequeueUser
      :: (Ord sessionId, MonadIO m) => TVar (UserQueue sessionId rep userId)
      -> TVar (AnnounceDeck sessionId rep userId)
      -> sessionId
      -> m (Maybe userId)
    dequeueUser p w i = liftIO $ do
      maybePair <- atomically $ do
        q <- readTVar p
        case q of
          [] -> return Nothing
          (pair:q') -> do
            writeTVar p q'
            return (Just pair)
      case maybePair of
        Nothing -> return Nothing
        Just (userId, ref) -> do
          atomically $ do
            m <- readTVar w
            let m' = appendAt m i ref
            writeTVar w m'
          return (Just userId)
      where
        appendAt m k a = Map.alter (\case Nothing -> Just [a]; Just as -> Just (a:as)) k m

    announceSession
      :: (Ord sessionId, MonadIO m)
      => TVar (AnnounceDeck sessionId rep userId)
      -> Starter sessionId rep userId
      -> m ()
    announceSession w starter = liftIO $ do
      refs <- atomically $ do
        m <- readTVar w
        let i = sSessionId starter
        case Map.lookup i m of
          Nothing -> return []
          Just refs -> do
            let m' = Map.delete i m
            writeTVar w m'
            return refs
      mapM_ (\ref -> putMVar ref starter) refs

-- Sessions

newSessions :: (MonadIO m, Ord sessionId) => m (Sessions sessionId userId rep input state terminal m)
newSessions = liftIO $ do
  w <- liftIO $ newTVarIO Map.empty
  return Sessions
    { sInsertSession = insertSession w
    , sFindSession = findSession w
    , sRemoveSession = removeSession w }
  where
    insertSession
      :: (Ord sessionId, MonadIO m)
      => TVar (Map sessionId (SessionRecord userId rep input state terminal m))
      -> (sessionId, SessionRecord userId rep input state terminal m)
      -> m ()
    insertSession w (sessionId, rec) = liftIO . atomically $ modifyTVar w (Map.insert sessionId rec)

    findSession
      :: (Ord sessionId, MonadIO m)
      => TVar (Map sessionId (SessionRecord userId rep input state terminal m))
      -> sessionId
      -> m (Maybe (SessionRecord userId rep input state terminal m))
    findSession w i = liftIO . atomically $ do
      m <- readTVar w
      return $ Map.lookup i m

    removeSession
      :: (Ord sessionId, MonadIO m)
      => TVar (Map sessionId (SessionRecord userId rep input state terminal m))
      -> sessionId
      -> m ()
    removeSession w i = liftIO . atomically $ modifyTVar w (Map.delete i)

-- Registry

newRegistryInMemory :: (MonadIO m, Ord userId) => m userId -> m (Registry userId user m)
newRegistryInMemory genUserId = do
  w <- liftIO $ newTVarIO Map.empty
  return Registry
    { rInsertUser = insertUser genUserId w
    , rGetUserById = getUserById w }
  where
    insertUser :: (Ord userId, MonadIO m) => m userId ->  TVar (Map userId user) -> user -> m userId
    insertUser genUserId w user = do
      userId <- genUserId
      liftIO . atomically $ do
        m <- readTVar w
        writeTVar w (Map.insert userId user m)
        return userId

    getUserById :: (Ord userId, MonadIO m) => TVar (Map userId user) -> userId -> m (Maybe user)
    getUserById w i = liftIO . atomically $ do
      m <- readTVar w
      return $ Map.lookup i m

-- Results

newResultsInMemory :: (MonadIO m, Ord sessionId) => m (Results sessionId rep userId state extra m)
newResultsInMemory = liftIO $ do
  w <- liftIO $ newTVarIO Map.empty
  return Results
    { rSaveResult = saveResult w
    , rFindResult = findResult w }
  where
    saveResult
      :: (Ord sessionId, MonadIO m)
      => TVar (Map sessionId (Result sessionId rep userId state extra))
      -> Result sessionId rep userId state extra
      -> m ()
    saveResult w r = liftIO . atomically . modifyTVar w $ Map.insert (sSessionId . rStarter $ r) r

    findResult
      :: (Ord sessionId, MonadIO m)
      => TVar (Map sessionId (Result sessionId rep userId state extra))
      -> sessionId
      -> m (Maybe (Result sessionId rep userId state extra))
    findResult w i = liftIO . atomically $ do
      m <- readTVar w
      return $ Map.lookup i m
