{-# LANGUAGE OverloadedStrings, TupleSections #-}

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
import Control.Error
import Control.Monad.Trans.Class (lift)

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
          runScript $ do 
            ip <- getIp 
            updateIp $ dnsparams os ip

dnsparams :: Opts -> S.ByteString -> SimpleQuery
dnsparams (Opts u p i) ip = 
    [ ("username", B.pack u)
    , ("password", B.pack p)
    , ("id", B.pack i)
    , ("ip", ip)
    ]

updateIp :: SimpleQuery -> Script ()
updateIp params = updateIp >>= codeToError
  where
    updateIp = lift . withOpenSSL $ do
        let r = renderSimpleQuery True params
        let url = "https://cp.dnsmadeeasy.com/servlet/updateip" <> r
        get url (\p _ -> return . getStatusCode $ p)

getIp :: Script S.ByteString
getIp = do
    (code, ip) <- lift $ get "http://bot.whatismyipaddress.com" handler
    codeToError code
    return ip
  where
    handler = \p i -> (getStatusCode p,) <$> concatHandler p i

codeToError :: StatusCode -> Script ()
codeToError x | x < 300 = return ()
              | x < 400 = left "unexpected redirect"
              | x < 500 = left "auth error"
              | otherwise = left "server error"




