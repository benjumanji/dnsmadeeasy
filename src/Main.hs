{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as B
import Data.Monoid ((<>))
import Network.Http.Client
import Network.HTTP.Types.URI (renderSimpleQuery, SimpleQuery)
import OpenSSL (withOpenSSL)
import Options.Applicative
import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams

data Opts = Opts
    { user :: String
    , pword :: String
    , id :: String
    }

optParser = Opts
    <$> strOption 
        ( long "username"  
       <> short 'u'
       <> metavar "USER"
       <> help "DNS made easy username")
    <*> strOption
        ( long "password"
       <> short 'p'
       <> metavar "PASSWORD"
       <> help "DNS made easy password")
    <*> strOption
        ( long "id"
       <> short 'i'
       <> metavar "RECORD"
       <> help "DNS made easy record id")

opts = info (helper <*> optParser) desc
  where
    desc = fullDesc <> progDesc "Update a dynamic dns record"
                    <> header "dnsmadeeasy - a dyndns record updater"

main :: IO ()
main = do os <- execParser opts 
          ip <- getIp 
          updateIp $ dnsparams os ip

dnsparams :: Opts -> S.ByteString -> SimpleQuery
dnsparams (Opts u p i) ip = 
    [ ("username", B.pack u)
    , ("password", B.pack p)
    , ("id", B.pack i)
    , ("ip", ip)
    ]

updateIp :: SimpleQuery -> IO ()
updateIp params = withOpenSSL $ do
    let r = renderSimpleQuery True params
    let url = "https://cp.dnsmadeeasy.com/servlet/updateip" <> r
    get url (\p _ -> print $ getStatusCode p)

getIp :: IO S.ByteString
getIp = get "http://bot.whatismyipaddress.com" concatHandler

