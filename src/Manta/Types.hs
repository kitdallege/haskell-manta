{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}

module Manta.Types
  ( MantaClientT
  , runMantaClientT
  , runMantaClientStderrLogging
  , runMantaClientStdoutLogging
  , runMantaClientNoLogging
  , MantaEnv(..)
  , FileMetadata(..)
  , MantaAPIError(..)
  , MantaSigner (..)
  , MantaSignerType (..)
  ) where
import           Control.Monad.Logger        (LoggingT, MonadLogger,
                                              NoLoggingT (..),
                                              runStderrLoggingT,
                                              runStdoutLoggingT)
import           Control.Monad.Catch        (MonadThrow, MonadCatch)
import           Control.Monad.Trans.Class   (MonadTrans)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Aeson                  (FromJSON (..), Value (..),
                                              withObject, (.:), (.:?))
import qualified GHC.Show
import           Network.HTTP.Client         (Manager)
import           Protolude

newtype MantaClientT m a = MantaClientT {
    unMantaClientT :: ReaderT MantaEnv m a
} deriving ( Functor
           , Applicative
           , Alternative
           , Monad
           , MonadIO
           , MonadTrans
           , MonadReader MantaEnv
           , MonadLogger
           , MonadThrow
           , MonadCatch)

runMantaClientT :: (MonadIO m, MonadLogger m, MonadBaseControl IO m) =>
                    MantaEnv -> MantaClientT m a -> m a
runMantaClientT conf action = runReaderT (unMantaClientT action) conf

runMantaClientNoLogging :: (MonadIO m) =>
                            MantaEnv -> MantaClientT (NoLoggingT IO) a -> m a
runMantaClientNoLogging conf action = liftIO $ runNoLoggingT $ runMantaClientT conf action

runMantaClientStderrLogging :: (MonadIO m) =>
                                MantaEnv -> MantaClientT (LoggingT IO) a -> m a
runMantaClientStderrLogging conf action = liftIO $ runStderrLoggingT $ runMantaClientT conf action

runMantaClientStdoutLogging :: (MonadIO m) =>
                                MantaEnv -> MantaClientT (LoggingT IO) a -> m a
runMantaClientStdoutLogging conf action = liftIO $ runStdoutLoggingT $ runMantaClientT conf action

--------------------------------------------------------------------
--TODO: Abstract over the HTTP backend eg: typeclass it.
-- mostly for testing, but also provides a nice separation of concerns
--------------------------------------------------------------------

-- TODO: lens for field accessors..
data MantaEnv = MantaEnv
    { msUrl     :: !Text
    , msAccount :: !Text
    , msKey     :: !Text
    , msManager :: Manager
    , msSigner  :: MantaSigner
    }

instance Show MantaEnv where
    show s = "MantaEnv { msUrl = " ++ show (msUrl s) ++ ", msAccount = " ++ show (msAccount s) ++ " }"

-- TODO: Nicer story for the objects you work with. lens would be nice
-- so that ya can take a list returned by listDirectory [MantaEntity]
-- and partition it out based on a 'type' field which is a sumtype
-- MantaEntityType = Object | Directory | SnapLink
-- MantaEntity
data FileMetadata = FileMetadata
    { fmName       :: !Text
    , fmType       :: !Text
    , fmMTime      :: !Text
    , fmDurability :: Maybe Integer
    -- extraAttributes :: Map Text Value ? either Value or we go Text
    } deriving (Show)

instance FromJSON FileMetadata where
    parseJSON (Object v) = FileMetadata <$>
                            v .:  "name" <*>
                            v .:  "type" <*>
                            v .:  "mtime" <*>
                            v .:? "durability"
    parseJSON _          = mzero


data MantaSignerType =
      MantaSignerTypePrivate
    | MantaSignerTypeAgent
    | MantaSignerTypeCustom
    deriving (Show)

data MantaSigner = MantaSigner
    { mantaSignerType           :: MantaSignerType
    , mantaSignerAlgorithm      :: Text -- RSA.HashInfo
    , mantaSignerFingerprint    :: Text
    , mantaSignerSigner         :: ByteString -> ByteString
    }

data MantaAPIError
    = AuthSchemeError Text
    | AuthorizationError Text
    | BadRequestError Text
    | ChecksumError Text
    | ConcurrentRequestError Text
    | ContentLengthError Text
    | ContentMD5MismatchError Text
    | EntityExistsError Text
    | InvalidArgumentError Text
    | InvalidAuthTokenError Text
    | InvalidCredentialsError Text
    | InvalidDurabilityLevelError Text
    | InvalidKeyIdError Text
    | InvalidJobError Text
    | InvalidLinkError Text
    | InvalidLimitError Text
    | InvalidSignatureError Text
    | InvalidUpdateError Text
    | DirectoryDoesNotExistError Text
    | DirectoryExistsError Text
    | DirectoryNotEmptyError Text
    | DirectoryOperationError Text
    | InternalError Text
    | JobNotFoundError Text
    | JobStateError Text
    | KeyDoesNotExistError Text
    | NoApiServersAvailableError Text
    | NotAcceptableError Text
    | NotEnoughSpaceError Text
    | LinkNotFoundError Text
    | LinkNotObjectError Text
    | LinkRequiredError Text
    | ParentNotDirectoryError Text
    | PreconditionFailedError Text
    | PreSignedRequestError Text
    | RequestEntityTooLargeError Text
    | RequestExpiredError Text
    | ResourceNotFoundError Text
    | RootDirectoryError Text
    | ServiceUnavailableError Text
    | SSLRequiredError Text
    | UploadTimeoutError Text
    | UserDoesNotExistError Text
    | OtherMantaError Text
    deriving (Show, Eq, Typeable)

instance Exception MantaAPIError

instance FromJSON MantaAPIError where
    parseJSON = withObject "MantaAPIError" $ \o -> do
        code <- o .: "code"
        message <- o .: "message"
        return $ codeToErr code message
      where
          codeToErr code = case (code :: Text) of
              "AuthScheme"             -> AuthSchemeError
              "Authorization"          -> AuthorizationError
              "AuthorizationFailed"    -> AuthorizationError
              "BadRequest"             -> BadRequestError
              "Checksum"               -> ChecksumError
              "ConcurrentRequest"      -> ConcurrentRequestError
              "ContentLength"          -> ContentLengthError
              "ContentMD5Mismatch"     -> ContentMD5MismatchError
              "EntityExists"           -> EntityExistsError
              "InvalidArgument"        -> InvalidArgumentError
              "InvalidAuthToken"       -> InvalidAuthTokenError
              "InvalidCredentials"     -> InvalidCredentialsError
              "InvalidDurabilityLevel" -> InvalidDurabilityLevelError
              "InvalidKeyId"           -> InvalidKeyIdError
              "InvalidJob"             -> InvalidJobError
              "InvalidLink"            -> InvalidLinkError
              "InvalidLimit"           -> InvalidLimitError
              "InvalidSignature"       -> InvalidSignatureError
              "InvalidUpdate"          -> InvalidUpdateError
              "DirectoryDoesNotExist"  -> DirectoryDoesNotExistError
              "DirectoryExists"        -> DirectoryExistsError
              "DirectoryNotEmpty"      -> DirectoryNotEmptyError
              "DirectoryOperation"     -> DirectoryOperationError
              "Internal"               -> InternalError
              "JobNotFound"            -> JobNotFoundError
              "JobState"               -> JobStateError
              "KeyDoesNotExist"        -> KeyDoesNotExistError
              "NoApiServersAvailable"  -> NoApiServersAvailableError
              "NotAcceptable"          -> NotAcceptableError
              "NotEnoughSpace"         -> NotEnoughSpaceError
              "LinkNotFound"           -> LinkNotFoundError
              "LinkNotObject"          -> LinkNotObjectError
              "LinkRequired"           -> LinkRequiredError
              "ParentNotDirectory"     -> ParentNotDirectoryError
              "PreconditionFailed"     -> PreconditionFailedError
              "PreSignedRequest"       -> PreSignedRequestError
              "RequestEntityTooLarge"  -> RequestEntityTooLargeError
              "RequestExpired"         -> RequestExpiredError
              "ResourceNotFound"       -> ResourceNotFoundError
              "RootDirectory"          -> RootDirectoryError
              "ServiceUnavailable"     -> ServiceUnavailableError
              "SSLRequired"            -> SSLRequiredError
              "UploadTimeout"          -> UploadTimeoutError
              "UserDoesNotExist"       -> UserDoesNotExistError
              _                        -> OtherMantaError
