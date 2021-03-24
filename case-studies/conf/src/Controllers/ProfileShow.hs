{-# LANGUAGE OverloadedStrings #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.ProfileShow where

import           Database.Persist.Sql           ( toSqlKey )
import           Data.Int                       ( Int64 )
import           Data.Text                      ( Text )
import           Text.Mustache                  ( (~>)
                                                , ToMustache(..)
                                                )
import qualified Text.Mustache.Types           as Mustache
import           Frankie

import           Storm.Core
import           Storm.Actions
import           Storm.Filters
import           Storm.Infrastructure
import           Storm.Templates
import           Storm.Frankie
import           Model

import           Helpers
import           Controllers

data ProfileData = ProfileData { name :: Text, affiliation:: Text,  email :: Maybe Text }

instance TemplateData ProfileData where
  templateFile = "profile.html.mustache"

  toMustache (ProfileData name affiliation email) =
    Mustache.object ["name" ~> name, "affiliation" ~> affiliation, "email" ~> email]

{-@ profileShow :: _ -> TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
profileShow :: Int64 -> Controller ()
profileShow uid = do
  let userId = toSqlKey uid
  viewer    <- requireAuthUser
  viewerId  <- project userId' viewer
  maybeUser <- selectFirst (userId' ==. userId)
  user      <- case maybeUser of
    Nothing   -> respondTagged notFound
    Just user -> return user
  name        <- project userName' user
  affiliation <- project userAffiliation' user
  isChair     <- chair viewer
  email       <- if isChair || viewerId == userId
    then do
      email <- project userEmail' user
      return (Just email)
    else return Nothing
  respondHtml $ ProfileData name affiliation email
