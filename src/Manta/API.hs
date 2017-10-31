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
import           Control.Monad.Logger       (MonadLogger, logDebug)
import           Data.Aeson                 (decode)
import           Data.ByteString.Lazy.Char8 (lines)
import           Data.List                  (lookup)
import           Network.HTTP.Client        (Response(..), httpLbs, newManager,
                                             parseRequest, Request(..))
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Protolude
import           System.Environment         (lookupEnv)

import           Manta.Types
import           Network.HTTP.Types (statusCode, methodPut)


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
    liftIO $ parseRequest (strConv Strict uri)

_performRequest :: (MonadIO m, MonadLogger m) => Request -> MantaClientT m (Response LByteString)
_performRequest req = do
    state <- ask
    $(logDebug) (show req)
    resp <- liftIO $ httpLbs req (msManager state)
    let headers = responseHeaders resp
    $(logDebug) (show headers)
    return resp

_request :: (MonadIO m, MonadLogger m) => FilePath -> MantaClientT m (Response LByteString)
_request path = _mkRequest path >>= _performRequest

data RequestArgs = RequestArgs
    {

    } deriving (Show)
-- For each 'api method' there is a Raw version which returns the Response
-- this is for 'power users' whom need/want access to the Http headers.
listDirectoryRaw :: (MonadIO m, MonadLogger m) => FilePath -> MantaClientT m (Response LByteString, [FileMetadata])
listDirectoryRaw path = do
  $(logDebug) ("List Diretory: " <> show path)
  resp <- _request path
  return (resp, catMaybes . mapMaybe decode . lines $ responseBody resp)

listDirectory :: (MonadIO m, MonadLogger m) => FilePath -> MantaClientT m [FileMetadata]
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
    let req' = req {requestHeaders=[("Content-Type", "application/json; type=directory")], method=methodPut}
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
