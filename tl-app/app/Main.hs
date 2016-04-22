{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}
module Main where

import           Control.Concurrent         (forkIO)
import           Control.Monad.Catch        (SomeException, catch)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Trans.Either (EitherT)
import           Data.Int                   (Int64)
import           Data.Proxy                 (Proxy (..))
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.IO          as TIO
import           GHC.Prim                   (Proxy#, proxy#)
import           GHC.TypeLits               (SomeSymbol, Symbol, someSymbolVal)
import           Lens.Micro                 (Lens', (&), (.~), (^.))
-- -- import Control.Monad.Except(ExceptT)
import           Data.ByteString.Lazy       (ByteString)
import           Data.Text.Lazy             (pack)
import           Data.Text.Lazy.Encoding    (encodeUtf8)
import qualified Language.Javascript.JQuery as JQ
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp   (run)
import           Servant
import           Servant.Docs

import           TL.Pers.DDL
import           TL.Pers.DML
import           TL.Pers.Sqlite.Sqlite      (Sqlite, sqlite)
import           TL.Servant.SimpleHtml
import           TL.Servant.Types
import           TL.Types
-- import TL.Form.Simple

import           T1

sql :: IO ()
sql = -- do
    runSession sqlite "test.db" -- (do
            createTab1
--            createTab2
--            createTab3

-- TODO обработка ошибок
-- TODO внешние ключи
-- TODO ? добавление пустого значения по умолчанию; добавление Just автоматом
-- TODO проверить добавление через def -- произвольный порядок полей
-- TODO транзакции
-- TODO: to make conduit (or pipe) for Select

main :: IO ()
main = do
     forkIO sql
     site
--
site = do
     forkIO $ JQ.file >>= TIO.readFile >>= TIO.writeFile "js/jq.js"
     mapM_ forkIO    [ mkJsAPI pTab1API'
--                     , mkJsAPI pTab2API'
--                     , mkJsAPI pTab3API'
                     ]
     run 8081 app

type MyAPI = Tab1API --  :<|> Tab2API :<|> Tab3API
myAPI = Proxy :: Proxy MyAPI
type DocsAPI = MyAPI
      :<|> "doc.md" :> Raw
      :<|> "static" :> Raw
api :: Proxy DocsAPI
api = Proxy
app = serve api serverD
runTestDB :: PersMonad Sqlite :~> EitherT ServantErr IO
runTestDB = Nat $ runSession sqlite "test.db"
serverD :: Server DocsAPI
serverD = server :<|> serveDocs :<|> serveDirectory "js"
  where
    serveDocs _ respond = respond $ responseLBS ok200 [plain] docsBS
    plain = ("Content-Type", "text/plain")
server = enter runTestDB serverTab1
     -- :<|>    enter runTestDB serverTab2
     -- :<|>    enter runTestDB serverTab3
docsBS :: ByteString
docsBS = encodeUtf8
    . pack
    . markdown
    $ docsWithIntros [intro] myAPI
  where intro = DocIntro "Welcome"
                 ["This is our super webservice's API.", "Enjoy!"]
instance ToCapture (Capture "id" Int64) where
  toCapture _ = DocCapture "id" "identity of record"
instance ToCapture (Capture "t1_id" Int64) where
  toCapture _ = DocCapture "t1_id" "identity of record in table t1"
instance ToCapture (Capture "t2_id" Int64) where
  toCapture _ = DocCapture "t2_id" "identity of record in table t2"
