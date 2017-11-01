{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Catch (catch)
import           Manta
import           Protolude hiding (catch)


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
        liftIO $ print ("Creating directory" :: Text)
        _ <- putDirectory "public/test2" `catch` (\e -> do
                                                        let err = (show (e :: MantaAPIError)) :: Text
                                                        putStr err
                                                        return ())
        liftIO $ print ("Created directory" :: Text)
    return ()
