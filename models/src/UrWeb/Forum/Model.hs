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

module UrWeb.Forum.Model
  ( migrateAll
  , mkUser
  , mkRootAdmin
  , mkForum
  , mkAcl
  , mkMessage
  , User
  , RootAdmin
  , Forum
  , Acl
  , Message
  , userId'
  , userName'
  , userPass'
  , rootAdminId'
  , rootAdminUser'
  , forumId'
  , forumName'
  , forumPublic'
  , aclId'
  , aclForum'
  , aclUser'
  , aclLevel'
  , messageId'
  , messageForum'
  , messageUser'
  , messageSubject'
  , messageBody'
  , messageTime'
  , UserId
  , RootAdminId
  , ForumId
  , AclId
  , MessageId
  )
where


import           Database.Persist               ( Key )
import           Database.Persist.TH            ( share
                                                , mkMigrate
                                                , mkPersist
                                                , sqlSettings
                                                , persistLowerCase
                                                )
import qualified Database.Persist              as Persist

import           Storm.Core



--------------------------------------------------------------------------------
-- | Inline
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- | Persistent
--------------------------------------------------------------------------------

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name String
  pass String
  

RootAdmin
  user UserId
  

Forum
  name String
  public Bool
  

Acl
  forum ForumId
  user UserId
  level String
  

Message
  forum ForumId
  user UserId
  subject String
  body String
  time Int
  
|]

--------------------------------------------------------------------------------
-- | Predicates
--------------------------------------------------------------------------------

{-@ measure isRootAdmin :: UserId -> Bool @-}

{-@ measure isForumAdmin :: UserId -> ForumId -> Bool @-}

{-@ measure canRead :: UserId -> ForumId -> Bool @-}

{-@ measure canWrite :: UserId -> ForumId -> Bool @-}

{-@ measure isPublic :: ForumId -> Bool @-}

--------------------------------------------------------------------------------
-- | Policies
--------------------------------------------------------------------------------

{-@ predicate IsRootAdmin USER = isRootAdmin (entityKey USER) @-}

{-@ predicate IsForumAdminA USER ACL = isForumAdmin (entityKey USER) (aclForum (entityVal ACL)) @-}

{-@ predicate CanReadF USER FORUM = canRead (entityKey USER) (entityKey FORUM) @-}

{-@ predicate IsPublicM MESSAGE = isPublic (messageForum (entityVal MESSAGE)) @-}

{-@ predicate CanReadM USER MESSAGE = canRead (entityKey USER) (messageForum (entityVal MESSAGE)) @-}

{-@ predicate CanWriteM USER MESSAGE = canWrite (entityKey USER) (messageForum (entityVal MESSAGE)) @-}

--------------------------------------------------------------------------------
-- | Records
--------------------------------------------------------------------------------

{-@ measure getJust :: Key record -> Entity record @-}

-- * User
{-@ mkUser ::
        x_0: String
     -> x_1: String
     -> StormRecord <{\row -> userName (entityVal row) == x_0 && userPass (entityVal row) == x_1},
                     {\_ _ -> True},
                     {\x_0 x_1 -> (x_0 == x_1)}>
                     (Entity User) User
  @-}
mkUser :: String -> String -> StormRecord (Entity User) User
mkUser x_0 x_1 = StormRecord (User x_0 x_1)

{-@ invariant {v: Entity User | v == getJust (entityKey v)} @-}



{-@ assume userId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) User UserId
  @-}
userId' :: EntityFieldWrapper (Entity User) User UserId
userId' = EntityFieldWrapper UserId

{-@ measure userName :: User -> String @-}

{-@ measure userNameCap :: Entity User -> Bool @-}

{-@ assume userName' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userName (entityVal row)},
                          {\field row -> field == userName (entityVal row)},
                          {\old -> userNameCap old},
                          {\old _ _ -> userNameCap old}>
                          (Entity User) User String
  @-}
userName' :: EntityFieldWrapper (Entity User) User String
userName' = EntityFieldWrapper UserName

{-@ measure userPass :: User -> String @-}

{-@ measure userPassCap :: Entity User -> Bool @-}

{-@ assume userPass' ::
      EntityFieldWrapper <{\x_0 x_1 -> (x_0 == x_1)},
                          {\row field -> field == userPass (entityVal row)},
                          {\field row -> field == userPass (entityVal row)},
                          {\old -> userPassCap old},
                          {\old _ _ -> userPassCap old}>
                          (Entity User) User String
  @-}
userPass' :: EntityFieldWrapper (Entity User) User String
userPass' = EntityFieldWrapper UserPass

-- * RootAdmin
{-@ mkRootAdmin ::
        x_0: UserId
     -> StormRecord <{\row -> rootAdminUser (entityVal row) == x_0},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) RootAdmin
  @-}
mkRootAdmin :: UserId -> StormRecord (Entity User) RootAdmin
mkRootAdmin x_0 = StormRecord (RootAdmin x_0)

{-@ invariant {v: Entity RootAdmin | v == getJust (entityKey v)} @-}

{-@ invariant {v: Entity RootAdmin | isRootAdmin (rootAdminUser (entityVal v))} @-}

{-@ assume rootAdminId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) RootAdmin RootAdminId
  @-}
rootAdminId' :: EntityFieldWrapper (Entity User) RootAdmin RootAdminId
rootAdminId' = EntityFieldWrapper RootAdminId

{-@ measure rootAdminUser :: RootAdmin -> UserId @-}

{-@ measure rootAdminUserCap :: Entity RootAdmin -> Bool @-}

{-@ assume rootAdminUser' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == rootAdminUser (entityVal row)},
                          {\field row -> field == rootAdminUser (entityVal row)},
                          {\old -> rootAdminUserCap old},
                          {\old _ _ -> rootAdminUserCap old}>
                          (Entity User) RootAdmin UserId
  @-}
rootAdminUser' :: EntityFieldWrapper (Entity User) RootAdmin UserId
rootAdminUser' = EntityFieldWrapper RootAdminUser

-- * Forum
{-@ mkForum ::
        x_0: String
     -> x_1: Bool
     -> StormRecord <{\row -> forumName (entityVal row) == x_0 && forumPublic (entityVal row) == x_1},
                     {\_ user -> IsRootAdmin user},
                     {\x_0 x_1 -> (forumPublic (entityVal x_0) || IsRootAdmin x_1 || CanReadF x_1 x_0)}>
                     (Entity User) Forum
  @-}
mkForum :: String -> Bool -> StormRecord (Entity User) Forum
mkForum x_0 x_1 = StormRecord (Forum x_0 x_1)

{-@ invariant {v: Entity Forum | v == getJust (entityKey v)} @-}

{-@ invariant {v: Entity Forum | (forumPublic (entityVal v)) => isPublic (entityKey v)} @-}

{-@ assume forumId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Forum ForumId
  @-}
forumId' :: EntityFieldWrapper (Entity User) Forum ForumId
forumId' = EntityFieldWrapper ForumId

{-@ measure forumName :: Forum -> String @-}

{-@ measure forumNameCap :: Entity Forum -> Bool @-}

{-@ assume forumName' ::
      EntityFieldWrapper <{\x_0 x_1 -> (forumPublic (entityVal x_0) || IsRootAdmin x_1 || CanReadF x_1 x_0)},
                          {\row field -> field == forumName (entityVal row)},
                          {\field row -> field == forumName (entityVal row)},
                          {\old -> forumNameCap old},
                          {\x_0 x_1 x_2 -> ((IsRootAdmin x_2)) => (forumNameCap x_0)}>
                          (Entity User) Forum String
  @-}
forumName' :: EntityFieldWrapper (Entity User) Forum String
forumName' = EntityFieldWrapper ForumName

{-@ measure forumPublic :: Forum -> Bool @-}

{-@ measure forumPublicCap :: Entity Forum -> Bool @-}

{-@ assume forumPublic' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == forumPublic (entityVal row)},
                          {\field row -> field == forumPublic (entityVal row)},
                          {\old -> forumPublicCap old},
                          {\x_0 x_1 x_2 -> ((IsRootAdmin x_2)) => (forumPublicCap x_0)}>
                          (Entity User) Forum Bool
  @-}
forumPublic' :: EntityFieldWrapper (Entity User) Forum Bool
forumPublic' = EntityFieldWrapper ForumPublic

-- * Acl
{-@ mkAcl ::
        x_0: ForumId
     -> x_1: UserId
     -> x_2: String
     -> StormRecord <{\row -> aclForum (entityVal row) == x_0 && aclUser (entityVal row) == x_1 && aclLevel (entityVal row) == x_2},
                     {\row user -> IsForumAdminA user row || IsRootAdmin user},
                     {\x_0 x_1 -> (entityKey x_1 == aclUser (entityVal x_0) || IsRootAdmin x_1 || IsForumAdminA x_1 x_0)}>
                     (Entity User) Acl
  @-}
mkAcl :: ForumId -> UserId -> String -> StormRecord (Entity User) Acl
mkAcl x_0 x_1 x_2 = StormRecord (Acl x_0 x_1 x_2)

{-@ invariant {v: Entity Acl | v == getJust (entityKey v)} @-}

{-@ invariant {v: Entity Acl | (aclLevel (entityVal v)) == "admin" => isForumAdmin (aclUser (entityVal v)) (aclForum (entityVal v))} @-}

{-@ invariant {v: Entity Acl | canRead (aclUser (entityVal v)) (aclForum (entityVal v))} @-}

{-@ invariant {v: Entity Acl | ((aclLevel (entityVal v)) == "admin" || (aclLevel (entityVal v)) == "write") => canWrite (aclUser (entityVal v)) (aclForum (entityVal v))} @-}

{-@ assume aclId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Acl AclId
  @-}
aclId' :: EntityFieldWrapper (Entity User) Acl AclId
aclId' = EntityFieldWrapper AclId

{-@ measure aclForum :: Acl -> ForumId @-}

{-@ measure aclForumCap :: Entity Acl -> Bool @-}

{-@ assume aclForum' ::
      EntityFieldWrapper <{\x_0 x_1 -> (entityKey x_1 == aclUser (entityVal x_0) || IsRootAdmin x_1 || IsForumAdminA x_1 x_0)},
                          {\row field -> field == aclForum (entityVal row)},
                          {\field row -> field == aclForum (entityVal row)},
                          {\old -> aclForumCap old},
                          {\x_0 x_1 x_2 -> ((IsRootAdmin x_2)) => (aclForumCap x_0)}>
                          (Entity User) Acl ForumId
  @-}
aclForum' :: EntityFieldWrapper (Entity User) Acl ForumId
aclForum' = EntityFieldWrapper AclForum

{-@ measure aclUser :: Acl -> UserId @-}

{-@ measure aclUserCap :: Entity Acl -> Bool @-}

{-@ assume aclUser' ::
      EntityFieldWrapper <{\x_0 x_1 -> (entityKey x_1 == aclUser (entityVal x_0) || IsRootAdmin x_1 || IsForumAdminA x_1 x_0)},
                          {\row field -> field == aclUser (entityVal row)},
                          {\field row -> field == aclUser (entityVal row)},
                          {\old -> aclUserCap old},
                          {\x_0 x_1 x_2 -> ((IsRootAdmin x_2) || (IsForumAdminA x_2 x_0)) => (aclUserCap x_0)}>
                          (Entity User) Acl UserId
  @-}
aclUser' :: EntityFieldWrapper (Entity User) Acl UserId
aclUser' = EntityFieldWrapper AclUser

{-@ measure aclLevel :: Acl -> String @-}

{-@ measure aclLevelCap :: Entity Acl -> Bool @-}

{-@ assume aclLevel' ::
      EntityFieldWrapper <{\x_0 x_1 -> (entityKey x_1 == aclUser (entityVal x_0) || IsRootAdmin x_1 || IsForumAdminA x_1 x_0)},
                          {\row field -> field == aclLevel (entityVal row)},
                          {\field row -> field == aclLevel (entityVal row)},
                          {\old -> aclLevelCap old},
                          {\x_0 x_1 x_2 -> ((IsRootAdmin x_2) || (IsForumAdminA x_2 x_0)) => (aclLevelCap x_0)}>
                          (Entity User) Acl String
  @-}
aclLevel' :: EntityFieldWrapper (Entity User) Acl String
aclLevel' = EntityFieldWrapper AclLevel

-- * Message
{-@ mkMessage ::
        x_0: ForumId
     -> x_1: UserId
     -> x_2: String
     -> x_3: String
     -> x_4: Int
     -> StormRecord <{\row -> messageForum (entityVal row) == x_0 && messageUser (entityVal row) == x_1 && messageSubject (entityVal row) == x_2 && messageBody (entityVal row) == x_3 && messageTime (entityVal row) == x_4},
                     {\row user -> IsRootAdmin user || CanWriteM user row},
                     {\x_0 x_1 -> (IsPublicM x_0 || CanReadM x_1 x_0 || IsRootAdmin x_1)}>
                     (Entity User) Message
  @-}
mkMessage :: ForumId -> UserId -> String -> String -> Int -> StormRecord (Entity User) Message
mkMessage x_0 x_1 x_2 x_3 x_4 = StormRecord (Message x_0 x_1 x_2 x_3 x_4)

{-@ invariant {v: Entity Message | v == getJust (entityKey v)} @-}



{-@ assume messageId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Message MessageId
  @-}
messageId' :: EntityFieldWrapper (Entity User) Message MessageId
messageId' = EntityFieldWrapper MessageId

{-@ measure messageForum :: Message -> ForumId @-}

{-@ measure messageForumCap :: Entity Message -> Bool @-}

{-@ assume messageForum' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == messageForum (entityVal row)},
                          {\field row -> field == messageForum (entityVal row)},
                          {\old -> messageForumCap old},
                          {\x_0 x_1 x_2 -> ((IsRootAdmin x_2)) => (messageForumCap x_0)}>
                          (Entity User) Message ForumId
  @-}
messageForum' :: EntityFieldWrapper (Entity User) Message ForumId
messageForum' = EntityFieldWrapper MessageForum

{-@ measure messageUser :: Message -> UserId @-}

{-@ measure messageUserCap :: Entity Message -> Bool @-}

{-@ assume messageUser' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == messageUser (entityVal row)},
                          {\field row -> field == messageUser (entityVal row)},
                          {\old -> messageUserCap old},
                          {\old _ _ -> messageUserCap old}>
                          (Entity User) Message UserId
  @-}
messageUser' :: EntityFieldWrapper (Entity User) Message UserId
messageUser' = EntityFieldWrapper MessageUser

{-@ measure messageSubject :: Message -> String @-}

{-@ measure messageSubjectCap :: Entity Message -> Bool @-}

{-@ assume messageSubject' ::
      EntityFieldWrapper <{\x_0 x_1 -> (IsPublicM x_0 || CanReadM x_1 x_0 || IsRootAdmin x_1)},
                          {\row field -> field == messageSubject (entityVal row)},
                          {\field row -> field == messageSubject (entityVal row)},
                          {\old -> messageSubjectCap old},
                          {\x_0 x_1 x_2 -> ((IsRootAdmin x_2) || (CanWriteM x_2 x_0)) => (messageSubjectCap x_0)}>
                          (Entity User) Message String
  @-}
messageSubject' :: EntityFieldWrapper (Entity User) Message String
messageSubject' = EntityFieldWrapper MessageSubject

{-@ measure messageBody :: Message -> String @-}

{-@ measure messageBodyCap :: Entity Message -> Bool @-}

{-@ assume messageBody' ::
      EntityFieldWrapper <{\x_0 x_1 -> (IsPublicM x_0 || CanReadM x_1 x_0 || IsRootAdmin x_1)},
                          {\row field -> field == messageBody (entityVal row)},
                          {\field row -> field == messageBody (entityVal row)},
                          {\old -> messageBodyCap old},
                          {\x_0 x_1 x_2 -> ((IsRootAdmin x_2) || (CanWriteM x_2 x_0)) => (messageBodyCap x_0)}>
                          (Entity User) Message String
  @-}
messageBody' :: EntityFieldWrapper (Entity User) Message String
messageBody' = EntityFieldWrapper MessageBody

{-@ measure messageTime :: Message -> Int @-}

{-@ measure messageTimeCap :: Entity Message -> Bool @-}

{-@ assume messageTime' ::
      EntityFieldWrapper <{\x_0 x_1 -> (IsPublicM x_0 || CanReadM x_1 x_0 || IsRootAdmin x_1)},
                          {\row field -> field == messageTime (entityVal row)},
                          {\field row -> field == messageTime (entityVal row)},
                          {\old -> messageTimeCap old},
                          {\x_0 x_1 x_2 -> ((CanWriteM x_2 x_0)) => (messageTimeCap x_0)}>
                          (Entity User) Message Int
  @-}
messageTime' :: EntityFieldWrapper (Entity User) Message Int
messageTime' = EntityFieldWrapper MessageTime
