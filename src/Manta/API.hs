{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Manta.API
  ( defEnv
  , showConfig
  , listDirectory
  , listDirectoryRaw
  , getFile
  , getFileRaw
  , putDirectory
  ) where
import           Control.Monad.Catch        (MonadThrow, throwM, MonadCatch)
import           Control.Monad.Logger       (MonadLogger, logDebug)
import           Data.Aeson                 (decode)
import           Data.ByteString.Lazy.Char8 (lines)
import qualified Network.HTTP.Types as HT
import qualified Network.HTTP.Client as HC
-- import           Network.HTTP.Client        (Request (..), Response (..),
--                                              httpLbs, newManager, parseRequest)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Protolude
import           System.Environment         (lookupEnv)
import           Manta.Types
import           Manta.Auth

-- import           Network.HTTP.Types         (RequestHeaders, methodPut, statusCode, hContentType, hDate, hUserAgent)

import qualified Paths_manta_client
import Data.Version (showVersion)
import qualified System.Info
-- import Data.UnixTime (getUnixTime, webDateFormat, formatUnixTime)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale, rfc822DateFormat )

defUserAgent :: ByteString
defUserAgent = strConv Strict $ "haskell-manta/" <>
    showVersion Paths_manta_client.version <> " (" <>
    System.Info.os <> " " <> System.Info.arch <>
    ") " <> System.Info.compilerName <> "-" <>
    showVersion System.Info.compilerVersion

defEnv :: MonadIO m => m MantaEnv
defEnv = do
  mgr <- liftIO $ HC.newManager tlsManagerSettings
  user <- liftIO $ lookupEnv "MANTA_USER"
  url <-  liftIO $ lookupEnv "MANTA_URL"
  key <-  liftIO $ lookupEnv "MANTA_KEY_ID"
  defSigner <- liftIO $ mkPrivateKeySigner (strConv Strict (fromMaybe mempty key))
  return MantaEnv
    { msUrl = strConv Strict $ fromMaybe mempty url
    , msAccount = strConv Strict $ fromMaybe mempty user
    , msKey = strConv Strict $ fromMaybe mempty key
    , msManager = mgr
    , msSigner = defSigner
    }

showConfig :: MonadIO m => MantaClientT m ()
showConfig = do
  config <- ask
  liftIO $ print config

_mkRequest :: (MonadIO m, MonadLogger m) => FilePath -> MantaClientT m HC.Request
_mkRequest path = do
    env <- ask
    let url = msUrl env
        acct = msAccount env
        uri = url <> "/" <> acct <> "/" <>  strConv Strict path
    req <- liftIO $ HC.parseRequest (strConv Strict uri)
    now <- liftIO $ do
        ct <- getCurrentTime
        let fmtString = "%a, %d %b %Y %H:%M:%S %Z"
            -- "%a, %d %b %Y %H:%M:%S GMT" ? python hardcodes GMT
            fmtTime = formatTime defaultTimeLocale fmtString ct
        return $ strConv Strict fmtTime
    signRequest ("date: " <> now) $ req {HC.requestHeaders=[
          (HT.hDate, now)
        , (HT.hUserAgent, defUserAgent)
        ]}

{-
headers: {
    'accept-encoding': 'gzip, deflate',
    'authorization': 'Signature keyId="/greenspun/keys/18:aa:10:bb:5b:b0:4d:7f:b8:7a:1c:73:8e:05:16:6f",algorithm="rsa-sha1",signature="N0TM6xeVzviUk0GNyA/nVt6/11ZpXYBN7uY6XURSknCQRLbg4+BY9xlyK19aAk71hOQ63Y81wgd+CTa3xIKO2QbyF/ILST5T9/STeq76nTvweAK1cXb4v3r6yK315XFg/B9R8qHNQuomrXDfvYVVH8IuzNrmyzxYDoVydT8jv1w+IqIEmeJIKLSiDHKv50Bd6Zi3JIHKRGLlki4wwQYn0DBSFXxozf2e4H/gF2P/eC4m8zyS7GUcLU4T3aeYj8g9yVXdGd3EiV6GTKrBD2eTf8VkPBHaELPFwWHOiukw7k/XWmSOVLm4Sl4HsPPxWR4VRtwBe4WKiUBngm4/DzJqSg=="',
    'date': 'Tue, 31 Oct 2017 16:27:03 GMT',
    'user-agent': 'python-manta/2.6.0 (linux2) Python/2.7.3'
}

-}
_performRequest :: (MonadIO m, MonadLogger m) => HC.Request -> MantaClientT m (HC.Response LByteString)
_performRequest req = do
    env <- ask
    $(logDebug) (show req)
    resp <- liftIO $ HC.httpLbs req (msManager env)
    $(logDebug) (show resp)
    return resp

_request :: (MonadIO m, MonadLogger m) => FilePath -> MantaClientT m (HC.Response LByteString)
_request path = _mkRequest path >>= _performRequest


-- For each 'api method' there is a Raw version which returns the Response
-- this is for 'power users' whom need/want access to the Http headers.
listDirectoryRaw :: (MonadIO m, MonadThrow m, MonadLogger m) => FilePath -> MantaClientT m (HC.Response LByteString, [FileMetadata])
listDirectoryRaw path = do
    $(logDebug) ("List Diretory: " <> show path)
    resp <- _request path
    statusCheck resp
    return (resp, catMaybes . mapMaybe decode . lines $ HC.responseBody resp)
  where
      statusCheck resp = unless ((HT.statusCode . HC.responseStatus $ resp) == 200) (throwManta resp)
      throwManta resp = do
          let merror = decode $ HC.responseBody resp :: Maybe MantaAPIError
          case merror of
              Nothing -> liftIO $ throwM $ OtherMantaError ("Response code of " <> show (HT.statusCode . HC.responseStatus $ resp) <> " body is not JSON parseable: " <> strConv Strict (HC.responseBody resp))
              Just err -> liftIO $ throwM err


listDirectory :: (MonadIO m, MonadThrow m, MonadLogger m) => FilePath -> MantaClientT m [FileMetadata]
listDirectory path = do
  (_, results) <- listDirectoryRaw path
  return results

getFileRaw :: (MonadIO m, MonadLogger m) => FilePath -> MantaClientT m (HC.Response LByteString, ByteString)
getFileRaw path = do
    $(logDebug) ("GetObject: " <> show path)
    resp <- _request path
    return (resp, strConv Strict $ HC.responseBody resp)

getFile :: (MonadIO m, MonadLogger m) => FilePath -> MantaClientT m ByteString
getFile path = do
  (_, results) <- getFileRaw path
  return results

putDirectory :: (MonadIO m, MonadCatch m, MonadLogger m) => FilePath -> MantaClientT m ()
putDirectory path = do
    req <- _mkRequest path
    let req' = addHeaders
                (req {HC.method=HT.methodPut})
                [(HT.hContentType, "application/json; type=directory")]
    resp <- _performRequest req'
    statusCheck resp
  where
      statusCheck :: MonadIO f => HC.Response LByteString -> f ()
      statusCheck resp = unless ((HT.statusCode . HC.responseStatus $ resp) == 204) (throwManta resp)
      throwManta resp = do
          let merror = decode $ HC.responseBody resp :: Maybe MantaAPIError
          case merror of
              Nothing -> liftIO $ throwM $ OtherMantaError ("Response code of " <> show (HT.statusCode . HC.responseStatus $ resp) <> " body is not JSON parseable: " <> strConv Strict (HC.responseBody resp))
              Just err -> liftIO $ throwM err

addHeaders :: HC.Request -> HT.RequestHeaders -> HC.Request
addHeaders req headers = let
    curHeaders = HC.requestHeaders req
    combinedHeaders = curHeaders <> headers
    in req {HC.requestHeaders=combinedHeaders}

signRequest :: (MonadIO m, MonadLogger m) => ByteString -> HC.Request -> MantaClientT m HC.Request
signRequest signstr req = do
    $(logDebug) $ strConv Strict ("sign '" <> signstr <> "'")
    env <- ask
    let account = msAccount env
        signer = msSigner env
        fingerprint = mantaSignerFingerprint signer
        algorithm = mantaSignerAlgorithm signer
        sign = mantaSignerSigner signer
        signature = sign signstr
        auth = strConv Strict $ "Signature keyId=\"/" <>
            account <> "/keys/" <>
            fingerprint <> "\",algorithm=\"" <>
            algorithm <> "\",signature=\"" <>
            strConv Lenient signature <> "\""
        req' = addHeaders req [(HT.hAuthorization, auth)]
    $(logDebug) $ strConv Strict ("signature: '" <> signature <> "'")
    $(logDebug) $ strConv Strict ("auth: '" <> auth <> "'")
    return req'
{-
if self.signer:
    # Signature auth.
    if "Date" not in headers:
        headers["Date"] = http_date()
    sigstr = 'date: ' + headers["Date"]
    algorithm, fingerprint, signature = self.signer.sign(sigstr)
    auth = 'Signature keyId="/%s/keys/%s",algorithm="%s",signature="%s"'\
           % ('/'.join(filter(None, [self.account, self.subuser])),
              fingerprint, algorithm, signature)
    headers["Authorization"] = auth
-}

{-
putObject
 * should set headers
  ** x-durability-level
  ** Content-Length
  ** Content-MD5 : base64encode+md5 digest


Java Client API:
delete
deleteRecursive
get
head
listObjects
put
putDirectory
pubSnapLink

js client api:
chattr
get
createReadStream
ftw 'find'
info
ln
ls
mkdir
mkdirp
put
createWriteStream
rmr
unlink
createJob
job
listJobs/jobs
addJobKey
cancelJob
endJob
jobInput
jobOutput
jobFailures
jobErrors
jobShare
signURL

py client api:
get
put
rm
ln
walk
ls
mkdir
mkdirp
stat
type
get_job
get_job_input
get_job_output
get_job_failures
get_job_errors

put_directory
list_directory
list_directory2
head_directory
delete_directory
put_object
get_object
get_object2
delete_object
put_snaplink
create_job
add_job_inputs
end_job_input
cancel_job
list_jobs
get_job
get_job_output
get_job_input
get_job_failures
get_job_errors


Ideally I able to provide both the raw 'filesystem' api
and a more 'idealized' graph api which hides the
implementation details of the raw api behind a more
semantic (get, put, del)? api

--}
