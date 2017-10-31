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
import           Control.Monad.Catch        (MonadThrow, throwM)
import           Control.Monad.Logger       (MonadLogger, logDebug)
import           Data.Aeson                 (decode, Value, eitherDecode')
import           Data.ByteString.Lazy.Char8 (lines)
import           Data.List                  (lookup)
import           Network.HTTP.Client        (Request (..), Response (..),
                                             httpLbs, newManager, parseRequest)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Protolude
import           System.Environment         (lookupEnv)
import Data.String (String)
import           Manta.Types
import           Network.HTTP.Types         (methodPut, statusCode, hContentType, hDate, hUserAgent)

import qualified Paths_manta_client
import Data.Version (showVersion)
import qualified System.Info

defUserAgent :: ByteString
defUserAgent = strConv Strict $ "haskell-manta/" <>
    showVersion Paths_manta_client.version <> " (" <>
    System.Info.os <> " " <> System.Info.arch <>
    ") " <> System.Info.compilerName <> "-" <>
    showVersion System.Info.compilerVersion

defEnv :: MonadIO m => m MantaEnv
defEnv = do
  mgr <- liftIO $ newManager tlsManagerSettings
  let signer _ =  True
  user <- liftIO $ lookupEnv "MANTA_USER"
  url <-  liftIO $ lookupEnv "MANTA_URL"
  key <-  liftIO $ lookupEnv "MANTA_KEY_ID"
  return MantaEnv
    { msUrl = strConv Strict $ fromMaybe mempty url
    , msAccount = strConv Strict $ fromMaybe mempty user
    , msKey = strConv Strict $ fromMaybe mempty key
    , msManager = mgr
    , msSigner = signer
    }

showConfig :: MonadIO m => MantaClientT m ()
showConfig = do
  config <- ask
  liftIO $ print config

_mkRequest :: (MonadIO m, MonadLogger m) => FilePath -> MantaClientT m Request
_mkRequest path = do
    state <- ask
    let url = msUrl state
        acct = msAccount state
        uri = url <> "/" <> acct <> "/" <>  strConv Strict path
    req <- liftIO $ parseRequest (strConv Strict uri)
    let req' = req {requestHeaders=[
          (hDate, "")
        , (hUserAgent, defUserAgent)
        ]}
    return req'
{-
headers: {
    'accept-encoding': 'gzip, deflate',
    'authorization': 'Signature keyId="/greenspun/keys/18:aa:10:bb:5b:b0:4d:7f:b8:7a:1c:73:8e:05:16:6f",algorithm="rsa-sha1",signature="N0TM6xeVzviUk0GNyA/nVt6/11ZpXYBN7uY6XURSknCQRLbg4+BY9xlyK19aAk71hOQ63Y81wgd+CTa3xIKO2QbyF/ILST5T9/STeq76nTvweAK1cXb4v3r6yK315XFg/B9R8qHNQuomrXDfvYVVH8IuzNrmyzxYDoVydT8jv1w+IqIEmeJIKLSiDHKv50Bd6Zi3JIHKRGLlki4wwQYn0DBSFXxozf2e4H/gF2P/eC4m8zyS7GUcLU4T3aeYj8g9yVXdGd3EiV6GTKrBD2eTf8VkPBHaELPFwWHOiukw7k/XWmSOVLm4Sl4HsPPxWR4VRtwBe4WKiUBngm4/DzJqSg=="',
    'date': 'Tue, 31 Oct 2017 16:27:03 GMT',
    'user-agent': 'python-manta/2.6.0 (linux2) Python/2.7.3'
}

-}
_performRequest :: (MonadIO m, MonadLogger m) => Request -> MantaClientT m (Response LByteString)
_performRequest req = do
    state <- ask
    $(logDebug) (show req)
    resp <- liftIO $ httpLbs req (msManager state)
    let headers = responseHeaders resp
    $(logDebug) (show resp)
    return resp

_request :: (MonadIO m, MonadLogger m) => FilePath -> MantaClientT m (Response LByteString)
_request path = _mkRequest path >>= _performRequest


-- For each 'api method' there is a Raw version which returns the Response
-- this is for 'power users' whom need/want access to the Http headers.
listDirectoryRaw :: (MonadIO m, MonadThrow m, MonadLogger m) => FilePath -> MantaClientT m (Response LByteString, [FileMetadata])
listDirectoryRaw path = do
    $(logDebug) ("List Diretory: " <> show path)
    resp <- _request path
    statusCheck resp
    return (resp, catMaybes . mapMaybe decode . lines $ responseBody resp)
  where
      statusCheck resp = unless ((statusCode . responseStatus $ resp) == 200) (throwManta resp)
      throwManta resp = do
          let merror = decode $ responseBody resp :: Maybe MantaAPIError
          case merror of
              Nothing -> liftIO $ throwM $ OtherMantaError ("Response code of " <> show (statusCode . responseStatus $ resp) <> " body is not JSON parseable: " <> strConv Strict (responseBody resp))
              Just err -> liftIO $ throwM err


listDirectory :: (MonadIO m, MonadThrow m, MonadLogger m) => FilePath -> MantaClientT m [FileMetadata]
listDirectory path = do
  (_, results) <- listDirectoryRaw path
  return results

getFileRaw :: (MonadIO m, MonadLogger m) => FilePath -> MantaClientT m (Response LByteString, ByteString)
getFileRaw path = do
    $(logDebug) ("GetObject: " <> show path)
    resp <- _request path
    return (resp, strConv Strict $ responseBody resp)

getFile :: (MonadIO m, MonadLogger m) => FilePath -> MantaClientT m ByteString
getFile path = do
  (_, results) <- getFileRaw path
  return results

putDirectory :: (MonadIO m, MonadLogger m) => FilePath -> MantaClientT m Bool
putDirectory path = do
    req <- _mkRequest path
    let req' = req {
          requestHeaders=[(hContentType, "application/json; type=directory")]
        , method=methodPut}
    resp <- _performRequest req'
    return $ (statusCode . responseStatus $ resp) == 204
{-
headers = {
            "Content-Type": "application/json; type=directory"
        }
        res, content = self._request(mdir, "PUT", headers=headers)
        if res["status"] != "204":
            raise errors.MantaAPIError(res, content)
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
