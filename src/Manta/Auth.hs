{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Manta.Auth
  ( loadKey
  , mkPrivateKeySigner
  , throwLeft
  ) where
import qualified Codec.Crypto.RSA            as RSA
import           Crypto.PubKey.OpenSsh       (OpenSshPrivateKey (..),
                                              decodePrivate)
import           Crypto.Types.PubKey.RSA     (PrivateKey)
import qualified Data.ByteString.Base64.Lazy as BSL64
import qualified Data.ByteString.Lazy        as BSL
import           Data.String                 (String)
import           Manta.Types
import           Protolude
import           System.Environment          (getEnv)

mkPrivateKeySigner :: Text -> IO MantaSigner
mkPrivateKeySigner fingerprint = do
    dir <- (++ "/.ssh/") <$> getEnv "HOME"
    Right key <- loadKey (dir ++ "id_rsa")
    let pkSigner = BSL.toStrict . BSL64.encode . RSA.rsassa_pkcs1_v1_5_sign RSA.hashSHA256 key . BSL.fromStrict
    return MantaSigner
        { mantaSignerType           = MantaSignerTypePrivate
        , mantaSignerAlgorithm      = "rsa-sha256"
        , mantaSignerFingerprint    = fingerprint
        , mantaSignerSigner         = pkSigner
        }

throwLeft :: Either String OpenSshPrivateKey -> Either Text PrivateKey
throwLeft (Right (OpenSshPrivateKeyRsa k)) = Right k
throwLeft (Right _)                        = Left "Wrong key type"
throwLeft (Left s)                         = Left $ "Error reading keys: " <> toS s

--readAndSign :: FilePath -> ByteString -> IO ByteString
--readAndSign file msg = (flip sign msg . throwLeft . decodePrivate) `fmap` readFile file

loadKey :: FilePath -> IO (Either Text PrivateKey)
loadKey p = (throwLeft . decodePrivate  . encodeUtf8) <$> readFile p
