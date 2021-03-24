{-# LANGUAGE OverloadedStrings #-}
module Server
    ( runServer
    )
where

import qualified Control.Concurrent.MVar       as MVar
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Reader           ( MonadReader(..)
                                                , ReaderT(..)
                                                )
import           Database.Persist.Sqlite        ( SqlBackend
                                                , runSqlite
                                                , runMigration
                                                )
import           Frankie.Config
import           Frankie.Auth
import qualified Database.Persist              as Persistent

import           Storm.Frankie
import           Storm.Core
import           Storm.Infrastructure
import           Storm.Filters
import           Storm.Insert
import           Storm.Updates
import           Storm.Actions

import           Controllers
import           Controllers.Wish               ( wishNew
                                                , wishShow
                                                , wishEdit
                                                )
import           Controllers.User               ( userShow )

import           Model


readConfig :: MonadIO m => m Config
readConfig = do
    templateCache <- liftIO $ MVar.newMVar mempty
    return $ Config { configTemplateCache = templateCache, configAuthMethod = httpAuthDb }

initDB :: IO ()
initDB = runSqlite "db.sqlite" (runMigration migrateAll)

httpAuthDb :: AuthMethod (Entity User) Controller
httpAuthDb = httpBasicAuth $ \username _password -> selectFirst (userName' ==. username)

runServer :: IO ()
runServer = runSqlite "db.sqlite" $ do
    backend <- ask
    cfg     <- readConfig
    liftIO . runFrankieServer "dev" $ do
        mode "dev" $ do
            host "localhost"
            port 3000
            initWithT $ mapTaggedT (configure cfg . flip runReaderT backend)
        dispatch $ do
            get "/user/:uid" userShow
            get "/wish"      wishNew
            post "/wish" wishNew
            get "/wish/:wid"      wishShow
            get "/wish/:wid/edit" wishEdit
            post "/wish/:wid/edit" wishEdit
            fallback $ respond notFound

{-@ LIQUID "--compile-spec" @-}
