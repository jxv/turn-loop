module TurnLoop where

-- import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Concurrent
-- import Control.Concurrent.STM
-- import Control.Concurrent.MVar
-- import Control.Concurrent.Chan

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

data Starter sessionId rep userId  = Starter
  { sSessionId :: sessionId
  , sUserIds :: Many rep userId
  }

data Registry userId user = Registry
  { rInsertUser :: user -> IO userId
  , rGetByUserId :: userId -> IO (Maybe user)
  }

data Lobby sessionId rep userId = Lobby
  { lTransferUser :: userId -> IO (Maybe (Starter sessionId rep userId))
  , lDequeueUser :: sessionId -> IO (Maybe userId)
  , lAnnounceSession :: Starter sessionId rep userId -> IO ()
  }

data SessionEntry rep input state terminal = SessionEntry
  { seThread :: Thread
  , seGames :: Many rep (Session input state terminal)
  }

data Session input state terminal = Session
  { sMove :: Chan input
  , sStep :: Chan (Step state terminal)
  }

data SessionRecord userId rep input state terminal = SessionRecord
  { srThread :: Thread
  , srLabeled :: Many rep (LabeledSession userId input state terminal)
  }

data LabeledSession userId input state terminal = LabeledSession
  { lsUserId :: userId
  , lsSession :: Session input state terminal
  }

data Sessions sessionId input state terminal = Sessions
  { sInsertSession :: (sessionId, Session input state terminal) -> IO ()
  , sFindSession :: sessionId -> IO (Maybe (Session input state terminal))
  , sRemoveSession :: sessionId -> IO ()
  }

data Results sessionId rep userId state extra = Results
  { rSaveResult :: Result sessionId rep userId state extra -> IO ()
  , rFindResult :: sessionId -> IO (Maybe (Result sessionId rep userId state extra))
  }

data Thread = Thread
  { tThreadId :: ThreadId
  , tKill :: IO ()
  }
