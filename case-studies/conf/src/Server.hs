{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Server
    ( runServer
    , initDB
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
                                                , createSqlitePool
                                                )
import           Frankie.Config
import           Frankie.Auth
import qualified Database.Persist              as Persistent

import           Storm.Core
import           Storm.Frankie
import           Storm.Infrastructure
import           Storm.Insert
import           Storm.Actions
import           Storm.Filters

import           Storm.Updates          -- TODO: DUMMY RECURSIVE IMPORTS for LH 

import           Controllers
import           Controllers.Paper
import           Controllers.Home
import           Model
import           Data.Pool                      ( Pool )
import qualified Data.Pool                     as Pool
import           Text.Mustache.Compile          ( TemplateCache )
import           Control.Monad.Base             ( MonadBase(..) )
import           Control.Monad.Trans.Control    ( MonadBaseControl(..)
                                                , MonadTransControl(..)
                                                , RunInBase
                                                )
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Logger           ( runNoLoggingT )



{-@ ignore runServer @-}
runServer :: IO ()
runServer = runNoLoggingT $ do
    pool  <- createSqlitePool "db.sqlite" 1

    cache <- liftIO $ MVar.newMVar mempty

    liftIO . runFrankieServer "dev" $ do
        mode "dev" $ do
            host "localhost"
            port 3000
            initWithT $ initFromPool (Config cache httpAuthDb) pool
        dispatch $ do
            get "/"      homeAuthor
            get "/paper" paperNew
            post "/paper" paperNew
            get "/paper/:pid"      paperShow
            get "/paper/:pid/edit" paperEdit
            post "/paper/:pid/edit" paperEdit

            get "/chair"            homeChair
            get "/chair/paper/:pid" paperChair
            post "/chair/paper/:pid" paperChair

            fallback $ respond notFound

httpAuthDb :: AuthMethod (Entity User) Controller
httpAuthDb = httpBasicAuth $ \username _password -> selectFirst (userName' ==. username)


initDB :: IO ()
initDB = runSqlite "db.sqlite" $ do
    runMigration migrateAll
    return ()

-- TODO find a way to provide this without exposing the instance of MonadBaseControl

initFromPool :: Config
             -> Pool SqlBackend
             -> Controller ()
             -> TaggedT (Entity User) (ControllerT TIO) ()
initFromPool cfg pool = mapTaggedT run
    where run act = Pool.withResource pool $ configure cfg . runReaderT act


instance MonadBase IO TIO where
    liftBase = TIO

instance MonadBaseControl IO TIO where
    type StM TIO a = a
    liftBaseWith f = TIO (f runTIO)
    restoreM = return

instance MonadBase IO (ControllerT TIO) where
    liftBase = lift . liftBase

instance MonadBaseControl IO (ControllerT TIO) where
    type StM (ControllerT TIO) a = ControllerStatus a
    liftBaseWith f = ControllerT $ \r -> TIO $ fmap Working (f (runTIO . flip runController r))
    restoreM st = ControllerT $ \_ -> return st
