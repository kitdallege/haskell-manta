{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where
import Protolude
import           Control.Monad.IO.Class (liftIO)
import           Manta

main :: IO ()
main = do
    print "Hello Manta!"
    signed <- privateKeySigner "foo"
    env <- defEnv
    print env
    runMantaClientStdoutLogging env $ do
        showConfig
        ls <- listDirectory "public/"
        liftIO $ do
            print "List Directory"
            mapM_ print ls
        contents <- getFile "public/dead.letter"
        liftIO $ do
            print "Contents of \"public/dead.letter\""
            print contents
        liftIO $ print "Creating directory"
        success <- putDirectory "public/test2"
        liftIO $ do
            print "putDirectory returned"
            print success
    return ()
