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

module UrWeb.Secret.Model
  ( migrateAll
  , mkUser
  , mkSecret
  , User
  , Secret
  , userId'
  , userName'
  , userPass'
  , secretId'
  , secretUser'
  , secretName'
  , secretValue'
  , UserId
  , SecretId
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
  

Secret
  user UserId
  name String
  value String
  
|]

--------------------------------------------------------------------------------
-- | Predicates
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- | Policies
--------------------------------------------------------------------------------



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
      EntityFieldWrapper <{\x_0 x_1 -> (x_0 == x_1)},
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

-- * Secret
{-@ mkSecret ::
        x_0: UserId
     -> x_1: String
     -> x_2: String
     -> StormRecord <{\row -> secretUser (entityVal row) == x_0 && secretName (entityVal row) == x_1 && secretValue (entityVal row) == x_2},
                     {\secret user -> secretUser (entityVal secret) == entityKey user},
                     {\x_0 x_1 -> (secretUser (entityVal x_0) == entityKey x_1)}>
                     (Entity User) Secret
  @-}
mkSecret :: UserId -> String -> String -> StormRecord (Entity User) Secret
mkSecret x_0 x_1 x_2 = StormRecord (Secret x_0 x_1 x_2)

{-@ invariant {v: Entity Secret | v == getJust (entityKey v)} @-}



{-@ assume secretId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Secret SecretId
  @-}
secretId' :: EntityFieldWrapper (Entity User) Secret SecretId
secretId' = EntityFieldWrapper SecretId

{-@ measure secretUser :: Secret -> UserId @-}

{-@ measure secretUserCap :: Entity Secret -> Bool @-}

{-@ assume secretUser' ::
      EntityFieldWrapper <{\x_0 x_1 -> (secretUser (entityVal x_0) == entityKey x_1)},
                          {\row field -> field == secretUser (entityVal row)},
                          {\field row -> field == secretUser (entityVal row)},
                          {\old -> secretUserCap old},
                          {\x_0 x_1 x_2 -> ((secretUser (entityVal x_0) == secretUser (entityVal x_1) && secretUser (entityVal x_0) == entityKey x_2)) => (secretUserCap x_0)}>
                          (Entity User) Secret UserId
  @-}
secretUser' :: EntityFieldWrapper (Entity User) Secret UserId
secretUser' = EntityFieldWrapper SecretUser

{-@ measure secretName :: Secret -> String @-}

{-@ measure secretNameCap :: Entity Secret -> Bool @-}

{-@ assume secretName' ::
      EntityFieldWrapper <{\x_0 x_1 -> (secretUser (entityVal x_0) == entityKey x_1)},
                          {\row field -> field == secretName (entityVal row)},
                          {\field row -> field == secretName (entityVal row)},
                          {\old -> secretNameCap old},
                          {\x_0 x_1 x_2 -> ((secretUser (entityVal x_0) == secretUser (entityVal x_1) && secretUser (entityVal x_0) == entityKey x_2)) => (secretNameCap x_0)}>
                          (Entity User) Secret String
  @-}
secretName' :: EntityFieldWrapper (Entity User) Secret String
secretName' = EntityFieldWrapper SecretName

{-@ measure secretValue :: Secret -> String @-}

{-@ measure secretValueCap :: Entity Secret -> Bool @-}

{-@ assume secretValue' ::
      EntityFieldWrapper <{\x_0 x_1 -> (secretUser (entityVal x_0) == entityKey x_1)},
                          {\row field -> field == secretValue (entityVal row)},
                          {\field row -> field == secretValue (entityVal row)},
                          {\old -> secretValueCap old},
                          {\x_0 x_1 x_2 -> ((secretUser (entityVal x_0) == secretUser (entityVal x_1) && secretUser (entityVal x_0) == entityKey x_2)) => (secretValueCap x_0)}>
                          (Entity User) Secret String
  @-}
secretValue' :: EntityFieldWrapper (Entity User) Secret String
secretValue' = EntityFieldWrapper SecretValue
