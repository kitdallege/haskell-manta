{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
module Manta.Client
    ( MonadManta
    , ls
    , ln
    , unlink
    , mkdir
    ) where
import           Protolude
import           Control.Monad.Catch          (MonadCatch)
import           Control.Monad.Logger         (MonadLogger)

import           Manta.Types
import Manta.API

type MonadManta m = (MonadIO m, MonadCatch m, MonadLogger m)


ls :: MonadManta m => FilePath -> MantaClientT m [MantaEntity]
ls = listDirectory

ln :: MonadManta m => FilePath -> FilePath -> MantaClientT m ()
ln = putSnapLink

unlink :: MonadManta m => FilePath -> MantaClientT m ()
unlink = deleteFile

mkdir :: MonadManta m => FilePath -> MantaClientT m ()
mkdir = putDirectory

-- get, put, rm all need the concept of `MantaEntityType`
-- so one could say
-- [get | rm] MantaEntityType FilePath
-- put MantaEntityType FilePath MantaEntityContent ?
-- rm Object filepath
--
-- ln, ls, mkdir, mkdirp, put, get, rm, stat
{-

Java Client API:
delete
deleteRecursive
get
head
listObjects
put
putDirectory
putSnapLink

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


cat            Download Manta objects and print them to stdout.
cd             change directory
cp             Copy files and dirs in Manta.
du             disk usage
exit           exit the shell
find           find paths
get            get a file from manta
head           Download Manta objects and print the first N lines to st...
help (?, man)
job            Run a Manta job
jobinfo        Get details for a Manta job.
jobs           List Manta jobs
json           Download a Manta objects and JSON pretty-print it. Note:...
lcd            change the local cwd
lls            local ls
ln             Create a snaplink to an object.
login          start a Manta compute login session
lpwd           print the local cwd
ls             list objects, directories and links
mkdir          create a directory
mv             move file(s)/dir(s) in manta
open           open the given manta path in your browser
put            put a local file to manta
pwd            print the cwd
rm             rm a file from manta
sign           Produce a signed URL for the given manta path.
tail           Download Manta objects and print the last N lines to std...
vi (vim)       edit a file on Manta (locally in vi)
zcat           Download Manta objects, gunzip and print them to stdout.


Ideally I able to provide both the raw 'filesystem' api
and a more 'idealized' graph api which hides the
implementation details of the raw api behind a more
semantic (get, put, del)? api

--}
