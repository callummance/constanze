{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Network.Constanze.CloudFlare where

import Network.Wreq
import Data.Aeson.Lens
import Control.Lens

import Control.Monad
import Control.Exception

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8

data ApiException = NoMatch
                  | ErrorReturned Int
                  | ManyMatching
                  | UnexpectedFormat
                  | UnknownZoneName
                  | MultipleMatchedZones
                  | MultipleMatchedHosts
                  | ServerFailure
                  deriving Show
instance Exception ApiException

data DnsEntry = DnsEntry { entryType :: String
                         , name      :: String
                         , content   :: String }
instance A.ToJSON DnsEntry where
  toJSON p = A.object [ "type"    A..= entryType p
                      , "name"    A..= name p
                      , "content" A..= content p ]

cloudFlareBaseAPI :: String
cloudFlareBaseAPI = "https://api.cloudflare.com/client/v4"

runUpdate :: String -> String -> String -> String -> String -> IO ()
runUpdate email apiKey ipAddr zoneName hostName
  = do
    updateResult <- (try $ updateDnsEntry email apiKey ipAddr zoneName hostName) :: IO (Either ApiException ())
    case updateResult of
      Left err    -> putStrLn $ "Failed to update hostname " ++ hostName ++ ", due to the error " ++ show err
      Right val   -> return val

updateDnsEntry :: String -> String -> String -> String -> String -> IO ()
updateDnsEntry email apiKey ipAddr zoneName hostName
  = do
    zoneId <- catch (getZoneId email apiKey zoneName) (\e -> case e of
      NoMatch       -> throwIO UnknownZoneName
      ManyMatching  -> throwIO MultipleMatchedZones
      otherErr      -> throwIO otherErr)
    dnsId <- try (getDnsEntryId email apiKey zoneId hostName)
    case dnsId of
      Left NoMatch        -> createApiDnsEntry email apiKey zoneId hostName ipAddr
      Left ManyMatching   -> throwIO MultipleMatchedHosts
      Left err            -> throwIO err
      Right cfId          -> updateApiDnsEntry email apiKey zoneId cfId hostName ipAddr


createApiDnsEntry :: String -> String -> String -> String -> String -> IO ()
createApiDnsEntry email apiKey zoneId hostName newVal
  = do
    let opts = authOpts email apiKey
    let url = cloudFlareBaseAPI ++ "/zones/" ++ zoneId ++ "/dns_records/"
    let payload = DnsEntry "A" hostName newVal
    res <- postWith opts url (A.toJSON payload)
    let responseCode = res ^. responseStatus . statusCode
    when (responseCode /= 200) $ throwIO (ErrorReturned responseCode)
    let success = res ^? responseBody . key "success" . _Bool
    case success of
      Nothing -> throwIO UnexpectedFormat
      Just s  -> unless s $ throwIO ServerFailure
    return ()

updateApiDnsEntry :: String -> String -> String -> String -> String -> String -> IO ()
updateApiDnsEntry email apiKey zoneId entryId hostName newVal
  = do
    let opts = authOpts email apiKey
    let url = cloudFlareBaseAPI ++ "/zones/" ++ zoneId ++ "/dns_records/" ++ entryId
    let payload = DnsEntry "A" hostName newVal
    res <- putWith opts url (A.toJSON payload)
    let responseCode = res ^. responseStatus . statusCode
    when (responseCode /= 200) $ throwIO (ErrorReturned responseCode)
    let success = res ^? responseBody . key "success" . _Bool
    case success of
      Nothing -> throwIO UnexpectedFormat
      Just s  -> unless s $ throwIO ServerFailure
    return ()


getDnsEntryId :: String -> String -> String -> String -> IO String
getDnsEntryId email apikey zoneId entryName
  = do
    let opts = authOpts email apikey & param "name" .~ [T.pack entryName] & param "page" .~ ["1"]
    zoneReq <- getWith opts (cloudFlareBaseAPI ++ "/zones/" ++ zoneId ++ "/dns_records")
    fetchId zoneReq

getZoneId :: String -> String -> String -> IO String
getZoneId email apikey zoneName
  = do
    let opts = authOpts email apikey & param "name" .~ [T.pack zoneName] & param "page" .~ ["1"]
    zoneReq <- getWith opts (cloudFlareBaseAPI ++ "/zones")
    fetchId zoneReq

fetchId :: Response L8.ByteString -> IO String
fetchId zoneReq
  = do
    let responseCode = zoneReq ^. responseStatus . statusCode
    when (responseCode /= 200) $ throwIO (ErrorReturned responseCode)
    let noResults = zoneReq ^? responseBody . key "result_info" . key "count" . _Integer
    case noResults of
      Nothing -> throwIO UnexpectedFormat
      Just 1  -> return ()
      Just 0  -> throwIO NoMatch
      Just _  -> throwIO ManyMatching
    case zoneReq ^? responseBody . key "result" . nth 0 . key "id" . _String of
      Nothing   -> throwIO UnexpectedFormat
      Just zid  -> return $ T.unpack zid

authOpts :: String -> String -> Options
authOpts email apikey
  = defaults & header "X-Auth-Email" .~ [B8.pack email]
             & header "X-Auth-Key"   .~ [B8.pack apikey]
             & header "Content-Type" .~ ["application/json"]
