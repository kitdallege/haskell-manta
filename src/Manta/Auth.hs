{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Manta.Auth
  ( privateKeySigner
  , loadKey
  ) where
import Protolude
import Data.Semigroup ((<>))
--import Manta.Types (Signer)
import           Codec.Crypto.RSA        (sign)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Crypto.PubKey.OpenSsh   (OpenSshPrivateKey (..), decodePrivate)
import           Crypto.Types.PubKey.RSA (PrivateKey)
-- import           Data.ByteString         (ByteString, readFile)
import           System.Environment      (getEnv)
-- import           Data.Text                   (Text)

signer :: Text -> Bool
signer key = ans
  where ans = True

privateKeySigner :: (MonadIO m) => Text -> m Bool
privateKeySigner key  = do
  liftIO $ do
    dir <- (++ "/.ssh/") <$> getEnv "HOME"
    key' <- loadKey (dir ++ "id_rsa")
    print ("creating signature for: " ++ dir)
    print key'
  return True

{-
import Codec.Crypto.RSA (sign)
import Crypto.PubKey.OpenSsh (decodePrivate, OpenSshPrivateKey)
import Crypto.Types.PubKey.RSA (PrivateKey)
import Data.ByteString (ByteString)

-}

throwLeft :: IsString s => Either s OpenSshPrivateKey -> Either Text PrivateKey
throwLeft (Right (OpenSshPrivateKeyRsa k)) = Right k
throwLeft (Right _) = Left "Wrong key type"
throwLeft (Left s)  = Left "Error reading keys: "

--readAndSign :: FilePath -> ByteString -> IO ByteString
--readAndSign file msg = (flip sign msg . throwLeft . decodePrivate) `fmap` readFile file

loadKey :: FilePath -> IO (Either Text PrivateKey)
loadKey p = (throwLeft . decodePrivate  . encodeUtf8) <$> readFile p

{-
k <- loadKey keyFile
let result = sign k msg


\> import qualified Codec.Crypto.RSA as RSA
\> import qualified Data.ByteString.Lazy.Char8 as LC
\> import Data.Char (ord)
\> privateKey <- loadKey "path/to/key"
\> let sign = RSA.rsassa_pkcs1_v1_5_sign RSA.ha_SHA1 privateKey
\> map ord . LC.unpack . sign . LC.pack $ "foo"
-}
