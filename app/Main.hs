{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Monad.IO.Class (liftIO)
import           Manta
import           Protolude


main :: IO ()
main = do
    print ("Hello Manta!" :: Text)
    env <- defEnv
    print env
    runMantaClientStdoutLogging env $ do
        showConfig
        ls <- listDirectory "public"
        liftIO $ do
            print ("List Directory" :: Text)
            mapM_ print ls
        -- contents <- getFile "public/dead.letter"
        -- liftIO $ do
        --     print ("Contents of \"public/dead.letter\"" :: Text)
        --     print contents
        -- liftIO $ print ("Creating directory" :: Text)
        -- success <- putDirectory "public/test2"
        -- liftIO $ do
        --     print ("putDirectory returned" :: Text)
        --     print success
        -- void $ listDirectory "publicz/"
    return ()
