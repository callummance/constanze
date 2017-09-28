{-# LANGUAGE OverloadedStrings #-}

module Network.Constanze (main) where

import Data.Aeson
import qualified Data.ByteString.Lazy as BS

import Network.Constanze.Ip
import Network.Constanze.CloudFlare

data Configuration = Configuration {
                        email :: String
                      , apiKey :: String
                      , updateTargetsList :: [UpdateTarget]
                      }
instance FromJSON Configuration where
  parseJSON = withObject "Configuration" $ \o ->
              Configuration <$> o .: "email" 
                            <*> o .: "apiKey"
                            <*> o .: "updateTargets"

data UpdateTarget = UpdateTarget {
                        zoneName  :: String
                      , hostNames :: [String]
                      }
instance FromJSON UpdateTarget where
  parseJSON = withObject "UpdateTarget" $ \o ->
              UpdateTarget <$> o .: "zoneName"
                           <*> o .: "hostnames"

confFile :: String
confFile = "/etc/constanze.json"

main :: IO ()
main
  = do
    confString <- BS.readFile confFile
    let conf = decode confString
    case conf of
      Just c -> updateTargets (email c) (apiKey c) (updateTargetsList c)
      Nothing -> putStrLn "Could not parse config file, aborting..."

updateTargets :: String -> String -> [UpdateTarget] -> IO ()
updateTargets apiEmail cfKey targets
  = do
    myIp <- getIpAddress
    let updates = foldr (\t ts -> ts ++ map (\hn -> (zoneName t, hn)) (hostNames t)) [] targets
    case myIp of
      Just ip   -> mapM_ (uncurry $ runUpdate apiEmail cfKey ip) updates
      Nothing   -> putStrLn "Could not discover external IP, aborting DNS update..."
