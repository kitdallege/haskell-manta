{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Manta.Auth
  ( privateKeySigner
  , loadKey
  , mkPrivateKeySigner
  ) where
import           Protolude
--import Manta.Types (Signer)
import qualified Codec.Crypto.RSA as RSA
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Crypto.PubKey.OpenSsh   (OpenSshPrivateKey (..), decodePrivate
                                         , OpenSshPublicKey (..), decodePublic)
import           Crypto.Types.PubKey.RSA (PrivateKey, PublicKey, KeyPair)
-- import           Data.ByteString         (ByteString, readFile)
import           System.Environment      (getEnv)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Base64.Lazy as BSL64
import Data.String (String)
import Manta.Types

mkPrivateKeySigner :: Text -> IO MantaSigner
mkPrivateKeySigner fingerprint = do
    dir <- (++ "/.ssh/") <$> getEnv "HOME"
    Right key <- loadKey (dir ++ "id_rsa")
    let pkSigner = BSL.toStrict . BSL64.encode . RSA.rsassa_pkcs1_v1_5_sign RSA.hashSHA256 key . BSL.fromStrict
    return $ MantaSigner
        { mantaSignerType           = MantaSignerTypePrivate
        , mantaSignerAlgorithm      = "rsa-sha256"
        , mantaSignerFingerprint    = fingerprint
        , mantaSignerSigner         = pkSigner
        }

signer :: Text -> Bool
signer key = ans
  where ans = True

privateKeySigner :: (MonadIO m) => Text -> m Bool
privateKeySigner fingerprint  = do
  liftIO $ do
    dir <- (++ "/.ssh/") <$> getEnv "HOME"
    Right key <- loadKey (dir ++ "id_rsa")
    let output = RSA.rsassa_pkcs1_v1_5_sign RSA.hashSHA1 key ""
    print ("creating signature for: " ++ dir)
    print output
    print key
  return True

{-
import Codec.Crypto.RSA (sign)
import Crypto.PubKey.OpenSsh (decodePrivate, OpenSshPrivateKey)
import Crypto.Types.PubKey.RSA (PrivateKey)
import Data.ByteString (ByteString)

-}

throwLeft :: Either String OpenSshPrivateKey -> Either Text PrivateKey
throwLeft (Right (OpenSshPrivateKeyRsa k)) = Right k
throwLeft (Right _)                        = Left "Wrong key type"
throwLeft (Left s)                         = Left $ "Error reading keys: " <> strConv Strict s

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
