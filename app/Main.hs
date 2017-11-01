{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Monad.IO.Class (liftIO)
-- import           Control.Monad.Catch (catch)
import           Manta
import           Protolude hiding (catch)


main :: IO ()
main = do
    print ("Hello Manta!" :: Text)
    env <- defEnv
    print env
    runMantaClientStdoutLogging env $ do
        -- showConfig
        -- contents <- getFile "public/dead.letter"
        -- liftIO $ do
        --     print ("Contents of \"public/dead.letter\"" :: Text)
        --     print contents
        -- liftIO $ print ("Creating directory" :: Text)
        -- _ <- putDirectory "public/test2" `catch` (\e -> do
        --                                                 let err = show (e :: MantaAPIError) :: Text
        --                                                 putStr err
        --                                                 return ())
        -- liftIO $ print ("Created directory" :: Text)

        -- liftIO $ do
        --     print ("List Directory" :: Text)
        --     mapM_ print ls
        liftIO $ print ("Putting a file." :: Text)
        _ <- putFile "/tmp/test.txt" "public/test2/test.txt"
        ls <- listDirectory "public/test2"
        liftIO $ do
            print ("List Directory" :: Text)
            mapM_ print ls
        contents <- getFile "public/test2/test.txt"
        liftIO $ do
            print ("Contents of \"public/test2/test.txt\"" :: Text)
            print contents
        void $ putMetadata "public/test2/test.txt" [("m-testing", "true")]
        ls' <- listDirectory "public/test2"
        void $ putSnapLink "public/test2/test3.txt" "public/test2/test2.txt"
        liftIO $ do
            print ("List Directory" :: Text)
            mapM_ print ls'
        return ()
    return ()
