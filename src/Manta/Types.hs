{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Manta.Types
  ( MantaClientT
  , runMantaClientT
  , runMantaClientStderrLogging
  , runMantaClientStdoutLogging
  , runMantaClientNoLogging
  , MantaEnv(..)
  , FileMetadata(..)
  , Signer
  ) where
import Protolude
import           Control.Monad.Logger        (LoggingT, MonadLogger,
                                              NoLoggingT (..),
                                              runStderrLoggingT, runStdoutLoggingT, runLoggingT)
import           Control.Monad.Trans.Class   (MonadTrans)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Aeson                  (FromJSON (..), Value (..), (.:),
                                              (.:?))
import           Network.HTTP.Client         (Manager)
import qualified GHC.Show

newtype MantaClientT m a = MantaClientT {
  unMantaClientT :: ReaderT MantaEnv m a
} deriving ( Functor
           , Applicative
           , Alternative
           , Monad
           , MonadIO
           , MonadTrans
           , MonadReader MantaEnv
           , MonadLogger)

runMantaClientT :: (MonadIO m, MonadLogger m, MonadBaseControl IO m) => MantaEnv -> MantaClientT m a -> m a
runMantaClientT conf action = runReaderT (unMantaClientT action) conf

runMantaClientNoLogging :: (MonadIO m) => MantaEnv -> MantaClientT (NoLoggingT IO) a -> m a
runMantaClientNoLogging conf action = liftIO $ runNoLoggingT $ runMantaClientT conf action

runMantaClientStderrLogging :: (MonadIO m) => MantaEnv -> MantaClientT (LoggingT IO) a -> m a
runMantaClientStderrLogging conf action = liftIO $ runStderrLoggingT $ runMantaClientT conf action

runMantaClientStdoutLogging :: (MonadIO m) => MantaEnv -> MantaClientT (LoggingT IO) a -> m a
runMantaClientStdoutLogging conf action = liftIO $ runStdoutLoggingT $ runMantaClientT conf action

data MantaEnv = MantaEnv
  { msUrl     :: !Text
  , msAccount :: !Text
  , msKey     :: !Text
  , msManager :: Manager
  , msSigner  :: Signer
  }

instance Show MantaEnv where
  show s = "MantaEnv { msUrl = " ++ show (msUrl s) ++ ", msAccount = " ++ show (msAccount s) ++ " }"

data FileMetadata = FileMetadata
  { fmName       :: !Text
  , fmType       :: !Text
  , fmMTime      :: !Text
  , fmDurability :: Maybe Integer
  } deriving (Show)

instance FromJSON FileMetadata where
  parseJSON (Object v) = FileMetadata <$>
                         v .:  "name" <*>
                         v .:  "type" <*>
                         v .:  "mtime" <*>
                         v .:? "durability"
  parseJSON _          = mzero


type Signer = Text -> Bool
