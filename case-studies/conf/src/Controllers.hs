{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers where

import           Control.Monad.Reader           ( ReaderT(..) )

import qualified Data.ByteString.Lazy          as ByteString
import           Data.Text.Encoding             ( encodeUtf8 )
import           Database.Persist.Sqlite        ( SqlBackend )
import qualified Control.Concurrent.MVar       as MVar
import           Frankie.Auth
import           Frankie.Config
import qualified Text.Mustache.Types           as Mustache


import           Storm.Core
import           Storm.Frankie
import           Storm.Infrastructure
import           Storm.Templates
import           Storm.Filters
import           Model

data Config = Config
  { configTemplateCache :: !(MVar.MVar Mustache.TemplateCache)
  , configAuthMethod :: !(AuthMethod (Entity User) Controller)
  }

type Controller = TaggedT (Entity User) (ReaderT SqlBackend (ConfigT Config (ControllerT TIO)))

instance HasTemplateCache Config where
  getTemplateCache = configTemplateCache

instance HasAuthMethod (Entity User) Controller Config where
  getAuthMethod = configAuthMethod

{-@ respondHtml :: _ -> TaggedT<{\_ -> True}, {\v -> v == currentUser 0}> _ _ _ @-}
respondHtml :: TemplateData d => d -> Controller ()
respondHtml d = do
  page <- renderTemplate d
  respondTagged . okHtml . ByteString.fromStrict . encodeUtf8 $ page
