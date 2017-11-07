{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Manta.API
  ( defEnv
  , showConfig
  -- Directories
  , listDirectory
  , listDirectoryRaw
  , putDirectory
  , deleteDirectory
  -- Objects
  , getFile
  , getFileRaw
  , putFile
  , putMetadata
  , deleteFile
  -- Snaplinks
  , putSnapLink
  -- *Debugging
  , logHttpConnection
  ) where
import           Control.Monad.Catch          (MonadCatch, MonadThrow, throwM)
import           Control.Monad.Logger         (MonadLogger, logDebug)
import           Data.Aeson                   (decode)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Base64.Lazy  as BSL64
import qualified Data.ByteString.Lazy         as BSL
import           Data.ByteString.Lazy.Char8   (lines)
import           Data.Version                 (showVersion)
import           Manta.Auth
import           Manta.Types
import qualified Network.HTTP.Client          as HC
import           Network.HTTP.Client.Internal (connectionWrite, mTlsConnection)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import qualified Network.HTTP.Types           as HT
import qualified Network.Mime                 as Mime
import qualified Paths_manta_client
import           Protolude
import           System.Environment           (lookupEnv)
import qualified System.Info
import qualified Data.Digest.Pure.MD5         as MD5
import           Data.Time.Clock              (getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import qualified System.FilePath.Posix        as FilePath

defUserAgent :: ByteString
defUserAgent = toS $ "haskell-manta/" <>
    showVersion Paths_manta_client.version <> " (" <>
    System.Info.os <> " " <> System.Info.arch <>
    ") " <> System.Info.compilerName <> "-" <>
    showVersion System.Info.compilerVersion

defEnv :: MonadIO m => m MantaEnv
defEnv = do
  mgr <-  liftIO $ HC.newManager tlsManagerSettings
  user <- liftIO $ lookupEnv "MANTA_USER"
  url <-  liftIO $ lookupEnv "MANTA_URL"
  key <-  liftIO $ lookupEnv "MANTA_KEY_ID"
  defSigner <- liftIO $ mkPrivateKeySigner (toS (fromMaybe mempty key))
  return MantaEnv
    { msUrl = toS $ fromMaybe mempty url
    , msAccount = toS $ fromMaybe mempty user
    , msKey = toS $ fromMaybe mempty key
    , msManager = mgr
    , msSigner = defSigner
    }

-- Directories
listDirectoryRaw :: (MonadIO m, MonadCatch m, MonadLogger m) =>
                FilePath ->
                MantaClientT m (HC.Response LByteString, [MantaEntity])
listDirectoryRaw path = do
    $(logDebug) ("List Diretory: " <> show path)
    resp <- _request path
    checkStatus resp 200
    return (resp, catMaybes . mapMaybe decode . lines $ HC.responseBody resp)

listDirectory :: (MonadIO m, MonadCatch m, MonadLogger m) =>
                FilePath -> MantaClientT m [MantaEntity]
listDirectory path = do
  (_, results) <- listDirectoryRaw path
  return results

putDirectory :: (MonadCatch m, MonadLogger m, MonadIO m) =>
                FilePath -> MantaClientT m ()
putDirectory path = do
    req <- _mkRequest path
    let req' = addHeaders
                (req {HC.method=HT.methodPut})
                [(HT.hContentType, "application/json; type=directory")]
    resp <- _performRequest req'
    checkStatus resp 204

deleteDirectory :: (MonadCatch m, MonadLogger m, MonadIO m) =>
                FilePath -> MantaClientT m ()
deleteDirectory  path = do
    req <- _mkRequest path
    let req' = req {HC.method=HT.methodDelete}
    resp <- _performRequest req'
    checkStatus resp 204

-- Files
--TODO: Either wrap an API like Http.RequestBody or provide methods for each
-- use-case (like putStream getStream putLbs etc.)
getFileRaw :: (MonadIO m,  MonadCatch m,  MonadLogger m) =>
                FilePath -> MantaClientT m (HC.Response LByteString, ByteString)
getFileRaw path = do
  $(logDebug) ("GetObject: " <> show path)
  resp <- _request path
  checkStatus resp 200
  return (resp, toS $ HC.responseBody resp)

getFile :: (MonadIO m,  MonadCatch m, MonadLogger m) =>
            FilePath -> MantaClientT m ByteString
getFile path = do
    (_, results) <- getFileRaw path
    return results


putFile :: (MonadIO m,  MonadCatch m, MonadLogger m) =>
            FilePath -> FilePath -> MantaClientT m ()
putFile localPath mantaPath  = do
    $(logDebug) ("PutObject: " <> show mantaPath)
    req <- _mkRequest mantaPath
    fileContents <- liftIO $ BSL.readFile localPath
    let ctype = Mime.defaultMimeLookup (toS (FilePath.takeFileName localPath))
        checksum = BSL64.encode . toS . MD5.md5DigestBytes . MD5.md5 $ fileContents
        req'  =  req {HC.method=HT.methodPut
                    , HC.requestBody=HC.RequestBodyLBS fileContents}
        req'' = addHeaders req' [
              (HT.hContentType, ctype)
            , ("x-durability-level", "2")
            , (HT.hContentMD5, toS checksum)
            ]
    resp <- _performRequest req''
    checkStatus resp 204

putMetadata :: (MonadIO m,  MonadCatch m, MonadLogger m) =>
                FilePath -> HT.RequestHeaders -> MantaClientT m ()
putMetadata mantaPath headers = do
    $(logDebug) ("PutMetadata: " <> show mantaPath)
    req <- _mkRequest mantaPath
    let headers' = [(HT.hContentType, "application/json")] <> headers
        req' = addHeaders (req {HC.method=HT.methodPut}) headers'
    resp <- _performRequest req'
    checkStatus resp 204

deleteFile :: (MonadIO m,  MonadCatch m, MonadLogger m) =>
                FilePath -> MantaClientT m ()
deleteFile mantaPath = do
    $(logDebug) ("DeleteObject: " <> show mantaPath)
    req <- (\r->r{HC.method=HT.methodDelete}) <$> _mkRequest mantaPath
    resp <- _performRequest req
    checkStatus resp 204


-- Snaplinks
putSnapLink :: (MonadIO m,  MonadCatch m, MonadLogger m) =>
                FilePath -> FilePath -> MantaClientT m ()
putSnapLink toPath fromPath = do
    env <- ask
    let acct = msAccount env
        fromPath' = "/" <> acct <> "/" <> toS fromPath
    $(logDebug) ("putSnapLink: " <> show toPath <> " -> " <> show fromPath')
    req <- (\r->r{HC.method=HT.methodPut}) <$> _mkRequest toPath
    let req' = addHeaders req [
                  (HT.hContentType, "application/json; type=link")
                , (HT.hLocation, toS fromPath')
                ]
    resp <- _performRequest req'
    checkStatus resp 204
    return ()

-- Jobs


-- Utils
_mkRequest :: (MonadIO m, MonadLogger m) =>
                FilePath -> MantaClientT m HC.Request
_mkRequest path = do
    env <- ask
    let url = msUrl env
        acct = msAccount env
        uri = url <> "/" <> acct <> "/" <>  toS path
    req <- liftIO $ HC.parseRequest (toS uri)
    now <- liftIO $ do
        ct <- getCurrentTime
        let fmtString = "%a, %d %b %Y %H:%M:%S %Z"
            -- "%a, %d %b %Y %H:%M:%S GMT" ? python hardcodes GMT
            fmtTime = formatTime defaultTimeLocale fmtString ct
        return $ toS fmtTime
    signRequest ("date: " <> now) $ req {HC.requestHeaders=[
          (HT.hDate, now)
        , (HT.hUserAgent, defUserAgent)
        , (HT.hAccept, "*/*")
        ],
        HC.redirectCount=0}

_performRequest :: (MonadIO m, MonadLogger m) =>
                    HC.Request -> MantaClientT m (HC.Response LByteString)
_performRequest req = do
    env <- ask
    $(logDebug) (show req)
    resp <- liftIO $ HC.httpLbs req (HC.getHttpManager env)
    $(logDebug) (show resp)
    return resp

_request :: (MonadIO m, MonadLogger m) =>
            FilePath -> MantaClientT m (HC.Response LByteString)
_request path = _mkRequest path >>= _performRequest


addHeaders :: HC.Request -> HT.RequestHeaders -> HC.Request
addHeaders req headers = let
    curHeaders = HC.requestHeaders req
    combinedHeaders = curHeaders <> headers
    in req {HC.requestHeaders=combinedHeaders}

signRequest :: (MonadIO m, MonadLogger m) =>
                ByteString -> HC.Request -> MantaClientT m HC.Request
signRequest signstr req = do
    $(logDebug) $ toS ("sign '" <> signstr <> "'")
    env <- ask
    let account = msAccount env
        signer = msSigner env
        fingerprint = mantaSignerFingerprint signer
        algorithm = mantaSignerAlgorithm signer
        sign = mantaSignerSigner signer
        signature = sign signstr
        auth = toS $ "Signature keyId=\"/" <>
            account <> "/keys/" <>
            fingerprint <> "\",algorithm=\"" <>
            algorithm <> "\",signature=\"" <>
            toSL signature <> "\""
        req' = addHeaders req [(HT.hAuthorization, auth)]
    $(logDebug) $ toS ("signature: '" <> signature <> "'")
    $(logDebug) $ toS ("auth: '" <> auth <> "'")
    return req'

checkStatus :: (MonadCatch m, MonadIO m) =>
                HC.Response LByteString -> Int -> m ()
checkStatus resp status = unless
        ((HT.statusCode . HC.responseStatus $ resp) == status) (throwManta resp)

throwManta :: (MonadThrow m, MonadIO m) => HC.Response LByteString -> m a
throwManta resp = do
    let merror = decode $ HC.responseBody resp :: Maybe MantaAPIError
    case merror of
        Nothing -> liftIO $ do
            putStr (HC.responseBody resp)
            throwM $ OtherMantaError (
                "Response code of " <> show (HT.statusCode . HC.responseStatus $ resp) <>
                " body is not JSON parseable: " <> toS (HC.responseBody resp))
        Just err -> liftIO $ throwM err

logHttpConnection :: HC.Manager -> HC.Manager
logHttpConnection mgr = mgr {mTlsConnection = \ha h p -> do
                connOrig <- mTlsConnection mgr ha h p
                return connOrig {connectionWrite = \bs -> do
                    BS.appendFile "/tmp/manta-client.log" bs
                    connectionWrite connOrig bs}}

showConfig :: MonadIO m => MantaClientT m ()
showConfig = ask >>= liftIO . print
