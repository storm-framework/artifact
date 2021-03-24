{-# LANGUAGE OverloadedStrings #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.Wish where

import           Database.Persist.Sql           ( toSqlKey
                                                , fromSqlKey
                                                )
import           Data.Int                       ( Int64 )
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )
import           Data.Function                  ( (&) )
import           Data.Maybe                     ( fromMaybe )
import           Data.ByteString                ( ByteString )
import           Text.Mustache                  ( (~>)
                                                , ToMustache(..)
                                                )
import qualified Text.Mustache.Types           as M
import           Text.Printf                    ( printf )
import           Frankie

import           Storm.Core
import           Storm.Actions
import           Storm.Updates
import           Storm.Insert
import           Storm.Filters
import           Storm.Helpers
import           Storm.Infrastructure
import           Storm.Templates
import           Storm.Frankie
import           Model

import           Helpers
import           Controllers

-----------------------------------------------------------------------------------
-- | New Wish
-----------------------------------------------------------------------------------

newtype WishNew = WishNew String

instance TemplateData WishNew where
  templateFile = "wish.edit.html.mustache"
  toMustache (WishNew action) = M.object
    ["action" ~> action, "accessLevels" ~> map (uncurry (getAccessLevel False)) accessLevels]


{-@ wishNew :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
wishNew :: Controller ()
wishNew = do
  viewer   <- requireAuthUser
  viewerId <- project userId' viewer
  req      <- request
  if reqMethod req == methodPost then insertWish viewerId else respondHtml (WishNew "/wish")


{-@ insertWish :: {v: UserId | v == entityKey (currentUser 0)} ->
  TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
insertWish :: UserId -> Controller ()
insertWish userId = do
  params <- parseForm
  let descr       = lookup "description" params
  let accessLevel = lookup "accessLevel" params & fmap Text.unpack
  case (descr, accessLevel) of
    (Just descr, Just accessLevel) -> do
      -- ENFORCE: userId == viewerId
      wishId <- insert (mkWish userId descr accessLevel)
      respondTagged (redirectTo (wishRoute wishId))
    _ -> respondTagged badRequest

-----------------------------------------------------------------------------------
-- | Edit Wish
-----------------------------------------------------------------------------------

data WishEdit = WishEdit String WishData

instance TemplateData WishEdit where
  templateFile = "wish.edit.html.mustache"

  toMustache (WishEdit action (WishData descr level)) = M.object
    [ "action" ~> action
    , "description" ~> descr
    , "accessLevels"
      ~> [ getAccessLevel (value == level) value label | (value, label) <- accessLevels ]
    ]


{-@ wishEdit :: _ -> TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
wishEdit :: Int64 -> Controller ()
wishEdit wid = do
  let wishId = toSqlKey wid
  req <- request
  whenT (reqMethod req == methodPost) (updateWish wishId)
  wishData <- getWishData wishId
  respondHtml (WishEdit (wishEditRoute wishId) wishData)


{-@ updateWish :: _ -> TaggedT<{\_ -> True}, {\_ -> True}> _ _ _ @-}
updateWish :: WishId -> Controller ()
updateWish id = do
  viewer   <- requireAuthUser
  viewerId <- project userId' viewer
  params   <- parseForm
  case (lookup "description" params, lookup "accessLevel" params) of
    (Just content, Just level) -> do
      let
        up =
          combine (wishDescription' `assign` content) (wishAccessLevel' `assign` Text.unpack level)
      updateWhere (wishId' ==. id &&: wishOwner' ==. viewerId) up
    (Just content, Nothing) -> do
      let up = wishDescription' `assign` content
      updateWhere (wishId' ==. id &&: wishOwner' ==. viewerId) up
    (Nothing, Just level) -> do
      let up = wishAccessLevel' `assign` Text.unpack level
      updateWhere (wishId' ==. id &&: wishOwner' ==. viewerId) up
    _ -> return ()

-----------------------------------------------------------------------------------
-- | Show Wish
-----------------------------------------------------------------------------------

newtype WishShow = WishShow WishData

instance TemplateData WishShow where
  templateFile = "wish.show.html.mustache"
  toMustache (WishShow wishData) = M.object ["wish" ~> wishData]

{-@ wishShow :: _ -> TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
wishShow :: Int64 -> Controller ()
wishShow wid = do
  let wishId = toSqlKey wid
  wishData <- getWishData wishId
  respondHtml (WishShow wishData)

{-@ showWishes :: _ -> TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
showWishes :: UserId -> Controller ()
showWishes ownerId = do
  viewer   <- requireAuthUser
  viewerId <- project userId' viewer
  let pub = wishAccessLevel' ==. "public"
  let chk = if viewerId == ownerId then trueF else pub
  let query = (wishOwner' ==. ownerId) &&: chk
  wishes <- selectList query
  descrs <- projectList wishDescription' wishes
  respondTagged notFound

-----------------------------------------------------------------------------------
-- | Misc
-----------------------------------------------------------------------------------

{-@ getWishData :: _ -> TaggedT<{\v -> currentUser 0 == v}, {\v -> True}> _ _ _ @-}
getWishData :: WishId -> Controller WishData
getWishData wishId = do
  viewer   <- requireAuthUser
  viewerId <- project userId' viewer
  wish     <- selectFirstOr404 (wishId' ==. wishId)

  level    <- project wishAccessLevel' wish
  owner    <- project wishOwner' wish
  friends  <- selectFirst
    (   friendshipUser1'
    ==. owner
    &&: friendshipUser2'
    ==. viewerId
    &&: friendshipStatus'
    ==. "accepted"
    )

  descr <- case (owner == viewerId, friends) of
    (True, _)             -> project wishDescription' wish
    (_, Just _) | level == "friends" -> project wishDescription' wish
    _ | level == "public" -> project wishDescription' wish
    _                     -> respondTagged forbidden

  return (WishData descr level)

wishRoute :: WishId -> ByteString
wishRoute wishId = encodeUtf8 (Text.pack path)
 where
  wid  = fromSqlKey wishId
  path = printf "/wish/%d" wid


wishEditRoute :: WishId -> String
wishEditRoute wishId = printf "/wish/%d/edit" wid where wid = fromSqlKey wishId


{-@ friendRequest :: {v: UserId | entityKey (currentUser 0) == v} -> UserId ->
  TaggedT<{\_ -> True}, {\_ -> True}> _ _ _ @-}
friendRequest :: UserId -> UserId -> Controller ()
friendRequest user1 user2 = do
  _ <- insert (mkFriendship user1 user2 "pending")
  return ()

{-@ acceptFriendship :: UserId -> {v: UserId | entityKey (currentUser 0) == v} ->
  TaggedT<{\_ -> True}, {\_ -> True}> _ _ _ @-}
acceptFriendship :: UserId -> UserId -> Controller ()
acceptFriendship user1 user2 = do
  let up = friendshipStatus' `assign` "accepted"
  let f  = friendshipUser1' ==. user1 &&: friendshipUser2' ==. user2
           -- &&: friendshipStatus ==. "pending"
  updateWhere f up

{-@ rejectFriendship :: FriendshipId -> TaggedT<{\_ -> True}, {\_ -> True}> _ _ _ @-}
rejectFriendship :: FriendshipId -> Controller ()
rejectFriendship friendshipId = do
  viewer     <- requireAuthUser
  viewerId   <- project userId' viewer
  friendship <- selectFirstOr404 (friendshipId' ==. friendshipId)
  user2      <- project friendshipUser2' friendship
  -- status     <- project friendshipStatus' friendship
  if user2 == viewerId -- && status == "pending"
    then do
      let up = friendshipStatus' `assign` "rejected"
      updateWhere (friendshipId' ==. friendshipId) up
    else respondTagged forbidden

-----------------------------------------------------------------------------------
-- | WishData
-----------------------------------------------------------------------------------

data WishData = WishData Text String

instance ToMustache WishData where
  toMustache (WishData descr level) = M.object ["description" ~> descr, "accessLevel" ~> level]

accessLevels :: [(String, String)]
accessLevels = [("private", "Private"), ("public", "Public"), ("friends", "Friends")]

getAccessLevel :: Bool -> String -> String -> M.Value
getAccessLevel isSelected value label = M.object
  ["value" ~> value, "label" ~> label, "selected" ~> (selected :: String)]
  where selected = if isSelected then "selected" else ""
