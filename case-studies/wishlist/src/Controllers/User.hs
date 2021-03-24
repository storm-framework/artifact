{-# LANGUAGE OverloadedStrings #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.User where

import           Database.Persist.Sql           ( toSqlKey
                                                , fromSqlKey
                                                )
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
import           Storm.Helpers
import           Storm.Infrastructure
import           Storm.Templates
import           Storm.Frankie
import           Model

import           Helpers
import           Controllers


newtype UserData = UserData [WishData]

instance TemplateData UserData where
    templateFile = "user.show.html.mustache"
    toMustache (UserData wishes) = Mustache.object ["wishes" ~> wishes]

data WishData = WishData { wishDataId :: WishId, wishDataDescription :: Text }

instance ToMustache WishData where
    toMustache (WishData id description) =
        Mustache.object ["id" ~> show (fromSqlKey id), "description" ~> description]

{-@ userShow :: Int64 -> TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
userShow :: Int64 -> Controller ()
userShow uid = do
    let userId = toSqlKey uid
    viewer   <- requireAuthUser
    viewerId <- project userId' viewer
    friends  <- selectFirst

       ((friendshipUser1' ==. userId)
        &&: (friendshipUser2' ==. viewerId)
        &&: (friendshipStatus' ==. "accepted")
       )
    wishesData <- case (viewerId == userId, friends) of
        (True, _) -> do
            wishes <- selectList (wishOwner' ==. userId)
            projectList2 (wishId', wishDescription') wishes
        (_, Just _) -> do
            wishes <- selectList
                (wishOwner' ==. userId &&: wishAccessLevel' <-. ["friends", "public"])
            projectList2 (wishId', wishDescription') wishes
        _ -> do
            wishes <- selectList (wishOwner' ==. userId &&: wishAccessLevel' ==. "public")
            projectList2 (wishId', wishDescription') wishes
    respondHtml (UserData (map (uncurry WishData) wishesData))
