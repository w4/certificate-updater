{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson
import qualified Data.ByteString.Char8 as B8
import Data.List (intercalate)
import Data.Time.Clock.POSIX
import Data.Time.Format
import Network.HTTP.Simple
import Network.HTTP.Types.Header (hAuthorization)
import Options.Generic
import RIO
import qualified RIO.ByteString.Lazy as BL
import RIO.FilePath
import System.Environment
import System.IO.Error (isDoesNotExistError)
import System.Posix

data Args w = Args
  { role :: w ::: String <?> "Vault role to assume" <#> "r",
    commonName :: w ::: String <?> "Common name to create certificate for" <#> "c",
    ipAddress :: w ::: String <?> "IP Address to associate with the certificate" <#> "i",
    outputDirectory :: w ::: FilePath <?> "Directory to write certificate and key to" <#> "o",
    url :: w ::: String <?> "Base URL of Vault" <#> "u",
    mount :: w ::: String <?> "Mount point of the CA to create certificate from" <#> "m"
  }
  deriving (Generic)

instance ParseRecord (Args Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

data Metadata = Metadata
  { expiration :: POSIXTime,
    generated :: POSIXTime
  }
  deriving (Generic, Show)

instance ToJSON Metadata where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Metadata

data AppEnv = AppEnv
  { appLogger :: !LogFunc,
    args :: Args Unwrapped
  }

instance HasLogFunc AppEnv where
  logFuncL = lens appLogger (\x y -> x {appLogger = y})

main :: IO ()
main = do
  logOptions <- logOptionsHandle stderr False
  withLogFunc logOptions $ \logFunc -> do
    appArgs <- unwrapRecord "certificate-updater"
    runRIO AppEnv {appLogger = logFunc, args = appArgs} createCertificate

createCertificate :: RIO AppEnv ()
createCertificate = do
  refreshNeeded <- certificateRequiresRefresh
  when refreshNeeded $ do
    vaultToken <- liftIO $ lookupEnv "VAULT_TOKEN"
    let (clientForRenew, clientForGenerate) = setupVaultClient (fromMaybe "" vaultToken)
    vaultRenewSelf clientForRenew
    generateAndSaveCertificate clientForGenerate

generateAndSaveCertificate :: (String -> VaultIssueCertificate -> RIO AppEnv VaultIssueCertificateResponse) -> RIO AppEnv ()
generateAndSaveCertificate clientForGenerate = do
  out <- vaultGenerateCertificate clientForGenerate
  metadata <- buildNewMetadata (certExpiration out)
  outDir <- asks (outputDirectory . args)
  liftIO $ do
    _ <- setFileCreationMask 0o007
    writeFile (outDir </> "ca-chain.crt") (intercalate "\n" (caChain out))
    writeFile (outDir </> "cert.crt") (certificate out)
    writeFile (outDir </> "key.pem") (privateKey out)
    BL.writeFile (outDir </> "metadata") (encode metadata)
  logSuccessfulRegeneratedCertificate out

buildNewMetadata :: POSIXTime -> RIO AppEnv Metadata
buildNewMetadata expr = do
  gen <- liftIO getPOSIXTime
  return Metadata {expiration = expr, generated = gen}

setupVaultClient :: (ToJSON a, FromJSON b, ToJSON c, FromJSON d) => String -> (String -> a -> RIO AppEnv b, String -> c -> RIO AppEnv d)
setupVaultClient vaultToken =
  let clientForRenew = vaultHttpRequest vaultToken
      clientForGenerate = vaultHttpRequest vaultToken
   in (clientForRenew, clientForGenerate)

vaultHttpRequest :: (ToJSON a, FromJSON b) => String -> String -> a -> RIO AppEnv b
vaultHttpRequest token endpoint payload = do
  base <- asks (url . args)
  let finalUrl = base <> "/v1/" <> endpoint
  logInfo $ fromString $ "Sending request to " <> finalUrl
  let request =
        setRequestBodyLBS (encode payload) $
          setRequestHeaders headers $
            setRequestMethod "POST" $
              parseRequestThrow_ finalUrl
  response <- httpJSON request
  unless (getResponseStatusCode response == 200) $ do
    throwString "Got non-200 response from Vault"
  return (getResponseBody response)
  where
    headers = [(hAuthorization, B8.pack $ "Bearer " ++ token), ("Content-Type", "application/json")]

vaultRenewSelf :: (String -> Value -> RIO AppEnv ()) -> RIO AppEnv ()
vaultRenewSelf client = do
  client "auth/token/renew-self" (object [])
  logInfo "Successfully renewed Vault token"

data VaultIssueCertificate = VaultIssueCertificate
  { common_name :: String,
    ip_sans :: String
  }
  deriving (Generic, Show)

instance ToJSON VaultIssueCertificate where
  toEncoding = genericToEncoding defaultOptions

newtype VaultIssueCertificateResponse = VaultIssueCertificateResponse {data_ :: VaultIssueCertificateResponseData} deriving (Generic, Show)

instance FromJSON VaultIssueCertificateResponse where
  parseJSON = withObject "VaultIssueCertificateResponse" $ \v ->
    VaultIssueCertificateResponse <$> v .: "data"

data VaultIssueCertificateResponseData = VaultIssueCertificateResponseData
  { caChain :: [String],
    certificate :: String,
    privateKey :: String,
    serialNumber :: String,
    privateKeyType :: String,
    certExpiration :: POSIXTime
  }
  deriving (Generic, Show)

instance FromJSON VaultIssueCertificateResponseData where
  parseJSON = withObject "VaultIssueCertificateResponseData" $ \v ->
    VaultIssueCertificateResponseData
      <$> v .: "ca_chain"
      <*> v .: "certificate"
      <*> v .: "private_key"
      <*> v .: "serial_number"
      <*> v .: "private_key_type"
      <*> v .: "expiration"

vaultGenerateCertificate :: (String -> VaultIssueCertificate -> RIO AppEnv VaultIssueCertificateResponse) -> RIO AppEnv VaultIssueCertificateResponseData
vaultGenerateCertificate client = do
  payload <-
    VaultIssueCertificate
      <$> asks (commonName . args)
      <*> asks (ipAddress . args)
  roleArg <- asks (role . args)
  mountPoint <- asks (mount . args)
  client (mountPoint <> "/issue/" <> roleArg) payload <&> data_

certificateRequiresRefresh :: RIO AppEnv Bool
certificateRequiresRefresh = do
  path <- asks (outputDirectory . args)
  result <- fetchCurrentMetadata path
  currentTime <- liftIO getPOSIXTime
  case result of
    Just metadata
      | hasPassedSafeRefreshInterval currentTime metadata -> logPassedTtlMessage currentTime metadata >> return True
      | otherwise -> logNotYetReadyMessage currentTime metadata >> return False
    Nothing -> do
      logInfo "Metadata does not exist, assuming this is our first run, continuing"
      return True

fetchCurrentMetadata :: FilePath -> RIO AppEnv (Maybe Metadata)
fetchCurrentMetadata path = do
  result <- try $ liftIO $ BL.readFile (path </> "metadata")
  case result of
    Left e
      | isDoesNotExistError e -> return Nothing
      | otherwise -> throwIO e
    Right content -> case decode content of
      Just metadata -> return (Just metadata)
      Nothing -> throwString "Failed to decode metadata"

hasPassedSafeRefreshInterval :: POSIXTime -> Metadata -> Bool
hasPassedSafeRefreshInterval currentTime (Metadata expr gen) =
  let expires = expr - currentTime
      totalTtl = expr - gen
   in expr < gen || (totalTtl - expires) / totalTtl >= 0.75

logPassedTtlMessage :: POSIXTime -> Metadata -> RIO AppEnv ()
logPassedTtlMessage currentTime (Metadata expr _) = do
  logInfo $
    display $
      "More than 3/4s through certificate TTL ("
        <> tshow (round (expr - currentTime) :: Integer)
        <> " seconds remaining), continuing"

logNotYetReadyMessage :: POSIXTime -> Metadata -> RIO AppEnv ()
logNotYetReadyMessage currentTime (Metadata expr gen) = do
  logWarn $
    display $
      "Not yet reached threshold for regeneration, "
        <> tshow (round (expr - currentTime) :: Integer)
        <> " seconds left of TTL "
        <> tshow (round (expr - gen) :: Integer)
        <> " seconds. "
        <> tshow (round ((expr - currentTime) - ((expr - gen) * 0.25)) :: Integer)
        <> " seconds until threshold is reached"

logSuccessfulRegeneratedCertificate :: VaultIssueCertificateResponseData -> RIO AppEnv ()
logSuccessfulRegeneratedCertificate d = do
  logInfo $
    fromString $
      "Successfully regenerated "
        <> privateKeyType d
        <> " certificate, new serial number is "
        <> serialNumber d
        <> " expiring at "
        <> formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" (posixSecondsToUTCTime (certExpiration d))
