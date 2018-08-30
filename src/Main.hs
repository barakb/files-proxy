{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}

-- :set -XOverloadedStrings
module Main where

import           Data.List                         (intercalate, sortBy)
import qualified Data.ByteString.Char8 as BS       (pack)
import           Network.Wai                       (Response, responseFile)
import           Network.Wai.Handler.Warp          (setPort, setLogger, defaultSettings, runSettings)
import           Network.Wai.Logger                (withStdoutLogger)
import           Servant
import           Network.HTTP.Types.Status         (status200)
import           Network.HTTP.Types.Header         (ResponseHeaders)
import           System.IO                         (FilePath)
import           System.IO.Unsafe                  (unsafePerformIO)
import qualified System.Directory as Dir           (getDirectoryContents, doesFileExist, removeFile)
import           Control.Monad.Reader              (MonadReader(..), ReaderT(..), MonadIO(..), asks)
import           Control.Monad.Except              (MonadError)
import           Control.Monad                     (filterM, forM)
import           Control.Concurrent.MVar           (MVar, readMVar, newMVar, modifyMVar)
import           Control.Concurrent.Async          (Async, async, wait)
import           Control.Concurrent                (threadDelay)
import           Control.Exception.Safe            (catchAny)
import qualified Data.Map.Strict as Map            (Map, fromList, lookup, insert, updateWithKey, elems, delete)
import           Crypto.Hash                       (SHA256(SHA256), hashWith)
import           Data.Monoid                       ((<>))
import           Data.Aeson                        (ToJSON(..), (.=), object)
import qualified Data.Aeson as Aeson               (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS (unpack)
import           Data.Int                          (Int64)



data BadRequestError = BadRequestError
    deriving (Eq, Read, Show)

type ServerAPI  = ("favicon.ico" :> Raw)
                  :<|> "download" :> CaptureAll "segments" String :> Get '[JSON] (String, String)
                  :<|> "list" :> Get '[JSON] [Value]

serverApi :: Proxy ServerAPI
serverApi = Proxy

server :: AppCtx -> Server ServerAPI
server ctx =  enter (runReaderTNat ctx :: ReaderT AppCtx Handler :~> Handler) $
              favicon
              :<|> download
              :<|> list


app :: AppCtx -> Application
app ctx = serve serverApi $ server ctx


-- download :: [Text] -> ReaderT AppCtx Handler (Text, Text)
download :: (MonadIO m, MonadError ServantErr m, MonadReader AppCtx m) => [String] -> m (String, String)
download parts = do
    let localPath = encode parts
    case localPath of
      Nothing -> throwError $ err412 { errBody = "Missing parameters"}
      Just key -> do
                   mvar <- asks cache
                   asyncFilePath <- liftIO $ modifyMVar mvar (getOrCreate key)
                   fp <- liftIO $ wait asyncFilePath
                   return (fp , intercalate "/" parts)
    where
      getOrCreate :: String -> Map.Map FilePath Value -> IO (Map.Map FilePath Value, Async FilePath)
      getOrCreate key m = case Map.lookup key m of
                            Just value -> do
                              nextTick <- applicationNextTick
                              return (Map.updateWithKey (setLMT nextTick) key m, future value)
                            Nothing -> do
                              nextTick <- applicationNextTick
                              downloadJob <- liftIO $ doDownload parts key
                              let val      = Value {name=key, future = downloadJob, lmt = nextTick}
                                  m'       = Map.insert key val m
                                  toRemove = surplusKeys m'
                              toRemove' <- filterM removeFile toRemove
                              return (foldr Map.delete m toRemove', downloadJob)
      setLMT :: Int64 ->  String -> Value -> Maybe Value
      setLMT tick _ val = Just val{lmt = tick}
      surplusKeys :: Map.Map FilePath Value -> [String]
      surplusKeys m = map name $ drop applicationMaxCached $ sortBy (flip compare) $ Map.elems m


-- serve favicon.ico from current directory
favicon :: Monad m => Tagged m Application
favicon  = Tagged $ return (\respond  ->  respond favicon')
           where favicon' = file "favicon.ico" [("Content-Type", "image/x-icon")]

list :: (MonadReader AppCtx m, MonadIO m) => m [Value]
list = do
  c <- asks cache
  liftIO $ sortBy (flip compare) . Map.elems <$> readMVar c

-- serve a file with the given headers.
file :: FilePath -> ResponseHeaders -> Response
file path headers = responseFile status200 headers path Nothing


applicationDir :: FilePath
applicationDir = "."

applicationPort :: Int
applicationPort = 8080

applicationMaxCached :: Int
applicationMaxCached = 2


applicationTick :: MVar Int64
{-# NOINLINE applicationTick #-}
applicationTick = unsafePerformIO $ newMVar 0

applicationNextTick :: IO Int64
applicationNextTick =  modifyMVar applicationTick $ \c -> return (c + 1, c)

main :: IO ()
main = do
    putStrLn $ "starting server at " <> show applicationPort
    withStdoutLogger $ \aplogger -> do
         m <- readMapFrom applicationDir
         ctx <- AppCtx <$> newMVar m
         let settings = setPort applicationPort $ setLogger aplogger defaultSettings
         runSettings settings $ app ctx


newtype AppCtx = AppCtx { cache :: MVar (Map.Map FilePath Value) }

readMapFrom :: FilePath -> IO (Map.Map FilePath Value)
readMapFrom dir = do
  keys <- directoryAsList dir
  values <- forM keys $ \key-> do
                    tick <- applicationNextTick
                    v <- async (return key)
                    return Value {name=key, future = v, lmt = tick}
  return $ Map.fromList $ zip keys values

-- List the files in a given directory,
directoryAsList :: FilePath -> IO [FilePath]
directoryAsList dir = do
  content <- Dir.getDirectoryContents dir
  filterM Dir.doesFileExist content

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

doDownload :: [String] -> FilePath -> IO (Async FilePath)
doDownload _ = async . \r -> do
                 threadDelay 2000000
                 return r

-- return true if file still exists after deletion
removeFile :: FilePath -> IO Bool
removeFile fileName = do
  Dir.removeFile fileName `catchAny` const (return ())
  Dir.doesFileExist fileName
