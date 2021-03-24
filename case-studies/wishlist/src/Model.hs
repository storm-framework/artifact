{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}


{-@ LIQUID "--compile-spec" @-}

module Model
  ( migrateAll
  , mkUser
  , mkWish
  , mkFriendship
  , User
  , Wish
  , Friendship
  , userId'
  , userName'
  , userUsername'
  , wishId'
  , wishOwner'
  , wishDescription'
  , wishAccessLevel'
  , friendshipId'
  , friendshipUser1'
  , friendshipUser2'
  , friendshipStatus'
  , UserId
  , WishId
  , FriendshipId
  )

where

import           Database.Persist               ( Key )
import           Database.Persist.TH            ( share
                                                , mkMigrate
                                                , mkPersist
                                                , sqlSettings
                                                , persistLowerCase
                                                )
import           Data.Text                      ( Text )
import qualified Database.Persist              as Persist

import           Storm.Core



share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name Text
  username Text


Wish
  owner UserId
  description Text
  accessLevel String


Friendship
  user1 UserId
  user2 UserId
  status String

|]

--------------------------------------------------------------------------------
-- | Predicates
--------------------------------------------------------------------------------

{-@ measure friends :: UserId -> UserId -> Bool @-}

--------------------------------------------------------------------------------
-- | Policies
--------------------------------------------------------------------------------

{-@ predicate PublicOrFriends WISH VIEWER = wishAccessLevel (entityVal WISH) == "public" || wishOwner (entityVal WISH) == entityKey VIEWER || (wishAccessLevel (entityVal WISH) == "friends" && friends (wishOwner (entityVal WISH)) (entityKey VIEWER)) @-}

{-@ predicate IsOwner WISH VIEWER = wishOwner (entityVal WISH) == entityKey VIEWER @-}

--------------------------------------------------------------------------------
-- | Records
--------------------------------------------------------------------------------

{-@ measure getJust :: Key record -> Entity record @-}

-- * User
{-@ mkUser ::
     x_0: Text
  -> x_1: Text
  -> StormRecord <
       {\row -> userName (entityVal row) == x_0 && userUsername (entityVal row) == x_1}
     , {\_ _ -> True}
     , {\x_0 x_1 -> False}
     > (Entity User) User
@-}
mkUser :: Text -> Text -> StormRecord (Entity User) User
mkUser x_0 x_1 = StormRecord (User x_0 x_1)

{-@ invariant {v: Entity User | v == getJust (entityKey v)} @-}



{-@ assume userId' :: EntityFieldWrapper <
    {\row viewer -> True}
  , {\row field  -> field == entityKey row}
  , {\field row  -> field == entityKey row}
  , {\_ -> False}
  , {\_ _ _ -> True}
  > (Entity User) User UserId
@-}
userId' :: EntityFieldWrapper (Entity User) User UserId
userId' = EntityFieldWrapper UserId

{-@ measure userName :: User -> Text @-}

{-@ measure userNameCap :: Entity User -> Bool @-}

{-@ assume userName' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == userName (entityVal row)}
  , {\field row -> field == userName (entityVal row)}
  , {\old -> userNameCap old}
  , {\old _ _ -> userNameCap old}
  > (Entity User) User Text
@-}
userName' :: EntityFieldWrapper (Entity User) User Text
userName' = EntityFieldWrapper UserName

{-@ measure userUsername :: User -> Text @-}

{-@ measure userUsernameCap :: Entity User -> Bool @-}

{-@ assume userUsername' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == userUsername (entityVal row)}
  , {\field row -> field == userUsername (entityVal row)}
  , {\old -> userUsernameCap old}
  , {\old _ _ -> userUsernameCap old}
  > (Entity User) User Text
@-}
userUsername' :: EntityFieldWrapper (Entity User) User Text
userUsername' = EntityFieldWrapper UserUsername

-- * Wish
{-@ mkWish ::
     x_0: UserId
  -> x_1: Text
  -> x_2: String
  -> StormRecord <
       {\row -> wishOwner (entityVal row) == x_0 && wishDescription (entityVal row) == x_1 && wishAccessLevel (entityVal row) == x_2}
     , {\wish viewer -> wishOwner (entityVal wish) == entityKey viewer}
     , {\x_0 x_1 -> (wishAccessLevel (entityVal x_0) == "public" || wishOwner (entityVal x_0) == entityKey x_1 || (wishAccessLevel (entityVal x_0) == "friends" && friends (wishOwner (entityVal x_0)) (entityKey x_1)))}
     > (Entity User) Wish
@-}
mkWish :: UserId -> Text -> String -> StormRecord (Entity User) Wish
mkWish x_0 x_1 x_2 = StormRecord (Wish x_0 x_1 x_2)

{-@ invariant {v: Entity Wish | v == getJust (entityKey v)} @-}



{-@ assume wishId' :: EntityFieldWrapper <
    {\row viewer -> True}
  , {\row field  -> field == entityKey row}
  , {\field row  -> field == entityKey row}
  , {\_ -> False}
  , {\_ _ _ -> True}
  > (Entity User) Wish WishId
@-}
wishId' :: EntityFieldWrapper (Entity User) Wish WishId
wishId' = EntityFieldWrapper WishId

{-@ measure wishOwner :: Wish -> UserId @-}

{-@ measure wishOwnerCap :: Entity Wish -> Bool @-}

{-@ assume wishOwner' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == wishOwner (entityVal row)}
  , {\field row -> field == wishOwner (entityVal row)}
  , {\old -> wishOwnerCap old}
  , {\x_0 x_1 x_2 -> ((False)) => (wishOwnerCap x_0)}
  > (Entity User) Wish UserId
@-}
wishOwner' :: EntityFieldWrapper (Entity User) Wish UserId
wishOwner' = EntityFieldWrapper WishOwner

{-@ measure wishDescription :: Wish -> Text @-}

{-@ measure wishDescriptionCap :: Entity Wish -> Bool @-}

{-@ assume wishDescription' :: EntityFieldWrapper <
    {\x_0 x_1 -> (wishAccessLevel (entityVal x_0) == "public" || wishOwner (entityVal x_0) == entityKey x_1 || (wishAccessLevel (entityVal x_0) == "friends" && friends (wishOwner (entityVal x_0)) (entityKey x_1)))}
  , {\row field -> field == wishDescription (entityVal row)}
  , {\field row -> field == wishDescription (entityVal row)}
  , {\old -> wishDescriptionCap old}
  , {\x_0 x_1 x_2 -> ((IsOwner x_0 x_2)) => (wishDescriptionCap x_0)}
  > (Entity User) Wish Text
@-}
wishDescription' :: EntityFieldWrapper (Entity User) Wish Text
wishDescription' = EntityFieldWrapper WishDescription

{-@ measure wishAccessLevel :: Wish -> String @-}

{-@ measure wishAccessLevelCap :: Entity Wish -> Bool @-}

{-@ assume wishAccessLevel' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == wishAccessLevel (entityVal row)}
  , {\field row -> field == wishAccessLevel (entityVal row)}
  , {\old -> wishAccessLevelCap old}
  , {\x_0 x_1 x_2 -> ((IsOwner x_0 x_2)) => (wishAccessLevelCap x_0)}
  > (Entity User) Wish String
@-}
wishAccessLevel' :: EntityFieldWrapper (Entity User) Wish String
wishAccessLevel' = EntityFieldWrapper WishAccessLevel

-- * Friendship
{-@ mkFriendship ::
     x_0: UserId
  -> x_1: UserId
  -> x_2: String
  -> StormRecord <
       {\row -> friendshipUser1 (entityVal row) == x_0 && friendshipUser2 (entityVal row) == x_1 && friendshipStatus (entityVal row) == x_2}
     , {\row user -> friendshipUser1 (entityVal row) == entityKey user && friendshipStatus (entityVal row) == "pending"}
     , {\x_0 x_1 -> False}
     > (Entity User) Friendship
@-}
mkFriendship :: UserId -> UserId -> String -> StormRecord (Entity User) Friendship
mkFriendship x_0 x_1 x_2 = StormRecord (Friendship x_0 x_1 x_2)

{-@ invariant {v: Entity Friendship | v == getJust (entityKey v)} @-}

{-@ invariant {v: Entity Friendship | (friendshipStatus (entityVal v)) == "accepted" => friends (friendshipUser1 (entityVal v)) (friendshipUser2 (entityVal v))} @-}

{-@ assume friendshipId' :: EntityFieldWrapper <
    {\row viewer -> True}
  , {\row field  -> field == entityKey row}
  , {\field row  -> field == entityKey row}
  , {\_ -> False}
  , {\_ _ _ -> True}
  > (Entity User) Friendship FriendshipId
@-}
friendshipId' :: EntityFieldWrapper (Entity User) Friendship FriendshipId
friendshipId' = EntityFieldWrapper FriendshipId

{-@ measure friendshipUser1 :: Friendship -> UserId @-}

{-@ measure friendshipUser1Cap :: Entity Friendship -> Bool @-}

{-@ assume friendshipUser1' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == friendshipUser1 (entityVal row)}
  , {\field row -> field == friendshipUser1 (entityVal row)}
  , {\old -> friendshipUser1Cap old}
  , {\x_0 x_1 x_2 -> ((False)) => (friendshipUser1Cap x_0)}
  > (Entity User) Friendship UserId
@-}
friendshipUser1' :: EntityFieldWrapper (Entity User) Friendship UserId
friendshipUser1' = EntityFieldWrapper FriendshipUser1

{-@ measure friendshipUser2 :: Friendship -> UserId @-}

{-@ measure friendshipUser2Cap :: Entity Friendship -> Bool @-}

{-@ assume friendshipUser2' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == friendshipUser2 (entityVal row)}
  , {\field row -> field == friendshipUser2 (entityVal row)}
  , {\old -> friendshipUser2Cap old}
  , {\x_0 x_1 x_2 -> ((False)) => (friendshipUser2Cap x_0)}
  > (Entity User) Friendship UserId
@-}
friendshipUser2' :: EntityFieldWrapper (Entity User) Friendship UserId
friendshipUser2' = EntityFieldWrapper FriendshipUser2

{-@ measure friendshipStatus :: Friendship -> String @-}

{-@ measure friendshipStatusCap :: Entity Friendship -> Bool @-}

{-@ assume friendshipStatus' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == friendshipStatus (entityVal row)}
  , {\field row -> field == friendshipStatus (entityVal row)}
  , {\old -> friendshipStatusCap old}
  , {\x_0 x_1 x_2 -> ((friendshipUser2 (entityVal x_0) == entityKey x_2 && (friendshipStatus (entityVal x_1) == "accepted" || friendshipStatus (entityVal x_1) == "rejected"))) => (friendshipStatusCap x_0)}
  > (Entity User) Friendship String
@-}
friendshipStatus' :: EntityFieldWrapper (Entity User) Friendship String
friendshipStatus' = EntityFieldWrapper FriendshipStatus

--------------------------------------------------------------------------------
-- | Inline
--------------------------------------------------------------------------------
