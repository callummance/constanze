module Network.Constanze.Ip (getIpAddress) where

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8

import qualified Data.Text as T
import Data.IP
import Data.Either
import Data.List

import Network.DNS.Resolver
import Network.DNS.Lookup
import Network.DNS.Types

import Network.Wreq
import Control.Lens

ipLookupAddresses :: [String]
ipLookupAddresses
  = [ "https://icanhazip.com"
    , "http://ipecho.net/plain"
    , "https://ifconfig.co/ip"
    , "https://api.ipify.org"
    ]

openDNSSettings :: (String, String)
openDNSSettings = ("myip.opendns.com", "208.67.222.222")





-- |Returns the current host's IP address
getIpAddress :: IO (Maybe String)
getIpAddress
  = do
    dnsRes <- getIpByDns
    case dnsRes of
      Left err    -> putStrLn err >> chooseIpFromHTTP
      Right res   -> return $ Just res

-- |Checks all HTTP IP address sources and chooses the most popular result
chooseIpFromHTTP :: IO (Maybe String)
chooseIpFromHTTP
  = do
    results <- getIPByHTTP
    mapM_ putStrLn $ lefts results
    let ipCounts = sortBy countSort $ countOccurences $ rights results
    mapM_ (putStrLn . (++) "An HTTP source returned a non-matching ip: " . show) $ tail ipCounts
    return $ if null ipCounts then Nothing else Just $ fst $ head ipCounts
      where
        countSort (_,x) (_,y)
          | x == y    = EQ
          | x <= y    = LT
          | x >= y    = GT
        countSort _ _ = EQ

countOccurences :: (Eq a) => [a] -> [(a, Int)]
countOccurences l
  = countOccurences' l []
    where
      countOccurences' [] counts     = counts
      countOccurences' (i:_) counts  = map (\(x,n) -> if i == x then (x, n+1) else (x, n)) counts

-- |Retrieves the external IP of the current host over HTTP
getIPByHTTP :: IO [Either String String]
getIPByHTTP
  = mapM getIpFromHTTPAddress ipLookupAddresses

getIpFromHTTPAddress :: String -> IO (Either String String)
getIpFromHTTPAddress fetchAddr
  = do
    resp <- get fetchAddr
    let respCode = resp ^. responseStatus . statusCode
    let respBody = T.unpack . T.strip . T.pack . L8.unpack $ resp ^. responseBody
    return $ if respCode == 200 then Right respBody else Left $ "Failed to fetch IP from " ++ fetchAddr ++ " with error response code " ++ show respCode

-- |Retrieves the external IP of the current host from the OpenDNS resolvers
getIpByDns :: IO (Either String String)
getIpByDns
  = do
    digResult <- uncurry dig openDNSSettings
    case digResult of
      Left err    -> return $ Left $ "Could not fetch IP via OpenDNS request, encountered error " ++ show err
      Right ip    -> return $ Right $ show $ head ip

-- |Given a name to look up and a server to look it up with, returns the IPv4 address of the given name
dig :: String -> String -> IO (Either DNSError [IPv4])
dig name server
  = do
    let hostname = B8.pack name
    rs <- getConf server >>= makeResolvSeed
    withResolver rs $ \resolver -> lookupA resolver hostname

  
getConf :: String -> IO ResolvConf
getConf server
  = return $ defaultResolvConf {resolvInfo = RCHostName server}
