
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}

-- :set -XOverloadedStrings
module Main where

import           Data.List                         (intercalate, sortBy)
import           Data.Maybe                        (listToMaybe, maybe)
import qualified Data.ByteString.Char8 as BS       (pack, unpack)
import           Network.Wai                       (Response, responseFile, responseLBS)
import           Network.Wai.Handler.Warp          (setPort, setLogger, defaultSettings, runSettings)
import           Network.Wai.Logger                (withStdoutLogger)
import           Servant
import           Network.HTTP.Types.Status         (status200)
import           Network.HTTP.Types.Header         (ResponseHeaders, hContentType)
import           System.IO                         (FilePath)
import           System.IO.Error                   (isAlreadyExistsError)
import qualified System.Directory as Dir           (getDirectoryContents, doesFileExist, removeFile, createDirectory)
import           Control.Monad.IO.Class            (MonadIO(..))
import           Control.Monad.Except              (MonadError)
import           Control.Monad                     (filterM, forM)
import           Control.Concurrent.MVar           (MVar, readMVar, newMVar, modifyMVar, modifyMVar_)
import           Control.Concurrent.Async          (Async, async, waitCatch)
import           Control.Exception.Safe            (catchAny, throwIO, catch)
import qualified Data.Map.Strict as Map            (Map, fromList, lookup, insert, updateWithKey, elems, delete)
import           Crypto.Hash                       (SHA256(SHA256), hashWith)
import           Network.HTTP.Types                (notFound404)
import           Data.Aeson                        (ToJSON(..), (.=), object)
import qualified Data.Aeson as Aeson               (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS (unpack, pack)
import           Data.Int                          (Int64)
import           Data.Conduit                      (runConduit, (.|))
import           Control.Monad.Trans.Resource      (runResourceT)
import           Data.Conduit.Binary               (sinkFile)
import           Network.HTTP.Client               (parseUrlThrow)
import           Network.HTTP.Simple               (getResponseBody, getResponseHeader, httpSource)
import           Numeric                           (showFFloat)
import           Options.Applicative
import           Data.Semigroup ((<>))

data BadRequestError = BadRequestError
    deriving (Eq, Read, Show)

type ServerAPI  = ("favicon.ico" :> Raw)
                  :<|> "download" :> CaptureAll "segments" String :> Raw
                  :<|> "list" :> Get '[JSON] [Value]

serverApi :: Proxy ServerAPI
serverApi = Proxy

server :: AppCtx -> Server ServerAPI
server ctx =  favicon
              :<|> download ctx
              :<|> list ctx



app :: AppCtx -> Application
app ctx = serve serverApi $ server ctx

download :: (MonadIO m, MonadError ServantErr m) => AppCtx -> [String] -> Tagged m Application
download ctx parts = let mvar        =  cache ctx
                         config      =  appConfig ctx
                         currentTick =  applicationTick ctx
                         in Tagged $ return (\respond  -> do
                              let localPath = encode parts
                              case localPath of
                                Nothing -> liftIO $ respond $ responseLBS notFound404 [(hContentType, "text/plain")] "Missing parameter"
                                Just key -> do
                                             let applicationMaxCached = cacheMaxsize config
                                                 downloadURL'         = remoteEndpoint config
                                                 cacheDir             = configDir config
                                             asyncFilePath <- liftIO $ modifyMVar mvar (getOrCreate key currentTick applicationMaxCached downloadURL' cacheDir)
                                             maybeFilePath <- liftIO $ waitCatch asyncFilePath
                                             case maybeFilePath of
                                               Right fp -> do
                                                 let localFile = configDir config <> "/" <> fp
                                                 liftIO $ putStrLn $ "Serving local file " <> localFile
                                                 respond $ file localFile [("Content-Type", "application/octet-stream")]
                                               Left err -> do
                                                 liftIO $ putStrLn $ "Error while trying to download  local file " <> key <> " " <> show err
                                                 liftIO $ modifyMVar_ mvar (delete key)
                                                 liftIO $ respond $ responseLBS notFound404 [(hContentType, "text/plain")] $ LBS.pack (show err))
    where
      getOrCreate :: String -> MVar Int64 -> Int -> String -> String -> Map.Map FilePath Value -> IO (Map.Map FilePath Value, Async FilePath)
      getOrCreate key currentTick applicationMaxCached downloadURL' cacheDir m = case Map.lookup key m of
                            Just val -> do
                              liftIO $ putStrLn $ "returns cached result for " <> key
                              nextTick <- applicationNextTick currentTick
                              return (Map.updateWithKey (setLMT nextTick) key m, future val)
                            Nothing -> do
                              liftIO $ putStrLn $ "downloading key " <> key <> " from url " <> downloadURL'
                              nextTick <- applicationNextTick currentTick
                              downloadJob <- liftIO $ doDownload downloadURL' parts cacheDir key
                              let val      = Value {name=key, future = downloadJob, lmt = nextTick}
                                  m'       = Map.insert key val m
                                  toRemove = surplusKeys applicationMaxCached m'
                              toRemove' <- filterM removeFile toRemove
                              return (foldr Map.delete m' toRemove', downloadJob)
      delete :: String ->  Map.Map FilePath Value -> IO (Map.Map FilePath Value)
      delete k m = return $ Map.delete k m
      setLMT :: Int64 ->  String -> Value -> Maybe Value
      setLMT tick _ val = Just val{lmt = tick}
      surplusKeys ::Int -> Map.Map FilePath Value -> [String]
      surplusKeys applicationMaxCached m = map name $ drop applicationMaxCached $ sortBy (flip compare) $ Map.elems m


-- serve favicon.ico from current directory
favicon :: Monad m => Tagged m Application
favicon  = Tagged $ return (\respond  ->  respond favicon')
           where favicon' = file "favicon.ico" [("Content-Type", "image/x-icon")]

list :: (MonadIO m) => AppCtx ->  m [Value]
list ctx = let c = cache ctx
           in liftIO $ sortBy (flip compare) . Map.elems <$> readMVar c

-- serve a file with the given headers.
file :: FilePath -> ResponseHeaders -> Response
file path headers = responseFile status200 headers path Nothing


applicationNextTick :: MVar Int64 -> IO Int64
applicationNextTick m =  modifyMVar m $ \c -> return (c + 1, c)

main :: IO ()
main = do
    config <- readConfig
    currentTick <- newMVar 0
    putStrLn $ "config: " <> show config
    m <- readMapFrom (configDir config) currentTick
    putStrLn $ "cache: " <> show m
    ctx <- AppCtx <$> newMVar m <*> return currentTick <*> return config
    withStdoutLogger $ \aplogger -> do
         let settings = setPort (port config) $ setLogger aplogger defaultSettings
         runSettings settings $ app ctx


data AppConfig = AppConfig {remoteEndpoint :: String, port :: Int, cacheMaxsize :: Int, configDir :: String} deriving Show

data AppCtx = AppCtx {cache :: MVar (Map.Map FilePath Value), applicationTick :: MVar Int64, appConfig :: AppConfig}


readConfig :: IO AppConfig
readConfig = let parser = AppConfig <$> strOption (long "url" <> short 'u' <> help "set the remote endpoint url, for example http://foo:3030/bar/download")
                                    <*> option auto (long "port" <> short 'p' <> help "listen a given port" <> showDefault <> value 8080 <> metavar "INT")
                                    <*> option auto (long "size" <> short 's' <> help "set the number of files saved in cache" <> showDefault <> value 200 <> metavar "INT")
                                    <*> strOption (long "dir" <> short 'd' <> help "set the location of the the cache dir on disk" <> showDefault <> value "./cache-dir")
                 desc =   fullDesc <> progDesc "proxy and cache for large zip files" <> header "proxy"
                 ops =    info (parser <**> helper) desc
             in  execParser ops

readMapFrom :: FilePath -> MVar Int64 ->  IO (Map.Map FilePath Value)
readMapFrom dir currentTick = do
  keys <- directoryAsList dir
  values <- forM keys $ \key-> do
                    nextTick <- applicationNextTick  currentTick
                    v <- async (return key)
                    return Value {name=key, future = v, lmt = nextTick}
  return $ Map.fromList $ zip keys values

-- List the files in a given directory,
directoryAsList :: FilePath -> IO [FilePath]
directoryAsList dir = do
  createDir dir
  content <- Dir.getDirectoryContents dir
  filterM (\c -> Dir.doesFileExist (dir <> "/" <> c)) content

encode :: [String] -> Maybe String
encode [] = Nothing
encode lst = let msg = intercalate "/" lst
                 sha = show $ hashWith SHA256 (BS.pack msg)
             in return $ sha  <> "-"  <> last lst

type LMT = Int64
data Value =  Value
    {   name :: FilePath
      , future :: Async FilePath
      , lmt :: LMT
    }

instance ToJSON Value where
    toJSON v = object ["name" .= name v, "lmt" .= lmt v]

instance Show Value where
    show = ("Value " <>) . LBS.unpack . Aeson.encode

instance Eq Value where
    v1 == v2 = name v1 == name v2

instance Ord Value where
    v1 <= v2 = lmt v1 <= lmt v2

doDownload :: String -> [String] -> String -> FilePath -> IO (Async FilePath)
doDownload baseURL pathComponents dir key = async $ do
                 let url = intercalate "/" $ baseURL : pathComponents
                     loc = dir <> "/" <> key
                 putStrLn $ "Downloading from " <> url <> " to " <> loc
                 downloadURL loc url key

-- return true if file still exists after deletion
removeFile :: FilePath -> IO Bool
removeFile fileName = do
  Dir.removeFile fileName `catchAny` const (return ())
  Dir.doesFileExist fileName

createDir :: FilePath -> IO ()
createDir dirName = Dir.createDirectory dirName `catch` handleExists
  where handleExists e
          | isAlreadyExistsError e = return ()
          | otherwise = throwIO e

----  Download file

downloadURL :: MonadIO m => String -> String -> String->  m String
downloadURL location url key = do
  request <- liftIO $ parseUrlThrow url
  liftIO $ runResourceT
         $ runConduit $  httpSource request processResponse
         .| sinkFile location
  return key
  where
     processResponse response = do
         _ <- liftIO $ putStrLn $ "Downloading " <> "[" <>  formatSize response <> "] " <> url
         getResponseBody response
     formatSize response = maybe "unknown" showAsMB (listToMaybe (getResponseHeader "Content-Length" response) >>= stringToInt . BS.unpack)

showAsMB ::Int -> String
showAsMB bytes = showFFloat (Just 2) ((fromIntegral bytes / (1024 ** 2))::Double) " MB"

stringToInt :: String -> Maybe Int
stringToInt cs =  case reads cs :: [(Int,String)] of
                      [(n, _)] -> Just n
                      _        -> Nothing
