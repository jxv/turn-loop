module TurnLoop.Types
  ( Many
  , Step(..)
  , Result(..)
  , Starter(..)
  , Registry(..)
  , Lobby(..)
  , SessionEntry(..)
  , Session(..)
  , SessionRecord(..)
  , LabeledSession(..)
  , Sessions(..)
  , Results(..)
  , Thread(..)
  ) where

import Control.Concurrent

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
