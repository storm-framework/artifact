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

module Hails.Gitstar.Model
  ( migrateAll
  , mkUser
  , mkApps
  , mkProject
  , mkReader
  , mkCollaborator
  , User
  , Apps
  , Project
  , Reader
  , Collaborator
  , userId'
  , userKeys'
  , appsId'
  , appsOwner'
  , appsName'
  , projectId'
  , projectOwner'
  , projectName'
  , projectPublic'
  , readerId'
  , readerUser'
  , readerProject'
  , collaboratorId'
  , collaboratorUser'
  , collaboratorProject'
  , UserId
  , AppsId
  , ProjectId
  , ReaderId
  , CollaboratorId
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
  keys String
  

Apps
  owner UserId
  name String
  

Project
  owner UserId
  name String
  public Bool
  

Reader
  user UserId
  project ProjectId
  

Collaborator
  user UserId
  project ProjectId
  
|]

--------------------------------------------------------------------------------
-- | Predicates
--------------------------------------------------------------------------------

{-@ measure isReader :: UserId -> ProjectId -> Bool @-}

{-@ measure isCollaborator :: UserId -> ProjectId -> Bool @-}

{-@ measure isOwner :: UserId -> ProjectId -> Bool @-}

--------------------------------------------------------------------------------
-- | Policies
--------------------------------------------------------------------------------

{-@ predicate IsReaderP USER PROJECT = isReader (entityKey USER) (entityKey PROJECT) @-}

{-@ predicate IsCollaboratorP USER PROJECT = isCollaborator (entityKey USER) (entityKey PROJECT) @-}

{-@ predicate IsOwnerR USER ROW = isOwner (entityKey USER) (readerProject (entityVal ROW)) @-}

{-@ predicate IsOwnerC USER ROW = isOwner (entityKey USER) (collaboratorProject (entityVal ROW)) @-}

--------------------------------------------------------------------------------
-- | Records
--------------------------------------------------------------------------------

{-@ measure getJust :: Key record -> Entity record @-}

-- * User
{-@ mkUser ::
        x_0: String
     -> StormRecord <{\row -> userKeys (entityVal row) == x_0},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) User
  @-}
mkUser :: String -> StormRecord (Entity User) User
mkUser x_0 = StormRecord (User x_0)

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

{-@ measure userKeys :: User -> String @-}

{-@ measure userKeysCap :: Entity User -> Bool @-}

{-@ assume userKeys' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userKeys (entityVal row)},
                          {\field row -> field == userKeys (entityVal row)},
                          {\old -> userKeysCap old},
                          {\x_0 x_1 x_2 -> ((entityKey x_2 == entityKey x_0)) => (userKeysCap x_0)}>
                          (Entity User) User String
  @-}
userKeys' :: EntityFieldWrapper (Entity User) User String
userKeys' = EntityFieldWrapper UserKeys

-- * Apps
{-@ mkApps ::
        x_0: UserId
     -> x_1: String
     -> StormRecord <{\row -> appsOwner (entityVal row) == x_0 && appsName (entityVal row) == x_1},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) Apps
  @-}
mkApps :: UserId -> String -> StormRecord (Entity User) Apps
mkApps x_0 x_1 = StormRecord (Apps x_0 x_1)

{-@ invariant {v: Entity Apps | v == getJust (entityKey v)} @-}



{-@ assume appsId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Apps AppsId
  @-}
appsId' :: EntityFieldWrapper (Entity User) Apps AppsId
appsId' = EntityFieldWrapper AppsId

{-@ measure appsOwner :: Apps -> UserId @-}

{-@ measure appsOwnerCap :: Entity Apps -> Bool @-}

{-@ assume appsOwner' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == appsOwner (entityVal row)},
                          {\field row -> field == appsOwner (entityVal row)},
                          {\old -> appsOwnerCap old},
                          {\old _ _ -> appsOwnerCap old}>
                          (Entity User) Apps UserId
  @-}
appsOwner' :: EntityFieldWrapper (Entity User) Apps UserId
appsOwner' = EntityFieldWrapper AppsOwner

{-@ measure appsName :: Apps -> String @-}

{-@ measure appsNameCap :: Entity Apps -> Bool @-}

{-@ assume appsName' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == appsName (entityVal row)},
                          {\field row -> field == appsName (entityVal row)},
                          {\old -> appsNameCap old},
                          {\x_0 x_1 x_2 -> ((appsOwner (entityVal x_0) == entityKey x_2)) => (appsNameCap x_0)}>
                          (Entity User) Apps String
  @-}
appsName' :: EntityFieldWrapper (Entity User) Apps String
appsName' = EntityFieldWrapper AppsName

-- * Project
{-@ mkProject ::
        x_0: UserId
     -> x_1: String
     -> x_2: Bool
     -> StormRecord <{\row -> projectOwner (entityVal row) == x_0 && projectName (entityVal row) == x_1 && projectPublic (entityVal row) == x_2},
                     {\row user -> entityKey user == projectOwner (entityVal row)},
                     {\x_0 x_1 -> (projectPublic (entityVal x_0) || IsCollaboratorP x_1 x_0 || IsReaderP x_1 x_0)}>
                     (Entity User) Project
  @-}
mkProject :: UserId -> String -> Bool -> StormRecord (Entity User) Project
mkProject x_0 x_1 x_2 = StormRecord (Project x_0 x_1 x_2)

{-@ invariant {v: Entity Project | v == getJust (entityKey v)} @-}

{-@ invariant {v: Entity Project | isOwner (projectOwner (entityVal v)) (entityKey v)} @-}

{-@ assume projectId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Project ProjectId
  @-}
projectId' :: EntityFieldWrapper (Entity User) Project ProjectId
projectId' = EntityFieldWrapper ProjectId

{-@ measure projectOwner :: Project -> UserId @-}

{-@ measure projectOwnerCap :: Entity Project -> Bool @-}

{-@ assume projectOwner' ::
      EntityFieldWrapper <{\x_0 x_1 -> (projectPublic (entityVal x_0) || IsCollaboratorP x_1 x_0 || IsReaderP x_1 x_0)},
                          {\row field -> field == projectOwner (entityVal row)},
                          {\field row -> field == projectOwner (entityVal row)},
                          {\old -> projectOwnerCap old},
                          {\old _ _ -> projectOwnerCap old}>
                          (Entity User) Project UserId
  @-}
projectOwner' :: EntityFieldWrapper (Entity User) Project UserId
projectOwner' = EntityFieldWrapper ProjectOwner

{-@ measure projectName :: Project -> String @-}

{-@ measure projectNameCap :: Entity Project -> Bool @-}

{-@ assume projectName' ::
      EntityFieldWrapper <{\x_0 x_1 -> (projectPublic (entityVal x_0) || IsCollaboratorP x_1 x_0 || IsReaderP x_1 x_0)},
                          {\row field -> field == projectName (entityVal row)},
                          {\field row -> field == projectName (entityVal row)},
                          {\old -> projectNameCap old},
                          {\x_0 x_1 x_2 -> ((entityKey x_2 == projectOwner (entityVal x_0))) => (projectNameCap x_0)}>
                          (Entity User) Project String
  @-}
projectName' :: EntityFieldWrapper (Entity User) Project String
projectName' = EntityFieldWrapper ProjectName

{-@ measure projectPublic :: Project -> Bool @-}

{-@ measure projectPublicCap :: Entity Project -> Bool @-}

{-@ assume projectPublic' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == projectPublic (entityVal row)},
                          {\field row -> field == projectPublic (entityVal row)},
                          {\old -> projectPublicCap old},
                          {\x_0 x_1 x_2 -> ((entityKey x_2 == projectOwner (entityVal x_0))) => (projectPublicCap x_0)}>
                          (Entity User) Project Bool
  @-}
projectPublic' :: EntityFieldWrapper (Entity User) Project Bool
projectPublic' = EntityFieldWrapper ProjectPublic

-- * Reader
{-@ mkReader ::
        x_0: UserId
     -> x_1: ProjectId
     -> StormRecord <{\row -> readerUser (entityVal row) == x_0 && readerProject (entityVal row) == x_1},
                     {\row user -> IsOwnerR user row},
                     {\x_0 x_1 -> False}>
                     (Entity User) Reader
  @-}
mkReader :: UserId -> ProjectId -> StormRecord (Entity User) Reader
mkReader x_0 x_1 = StormRecord (Reader x_0 x_1)

{-@ invariant {v: Entity Reader | v == getJust (entityKey v)} @-}

{-@ invariant {v: Entity Reader | isReader (readerUser (entityVal v)) (readerProject (entityVal v))} @-}

{-@ assume readerId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Reader ReaderId
  @-}
readerId' :: EntityFieldWrapper (Entity User) Reader ReaderId
readerId' = EntityFieldWrapper ReaderId

{-@ measure readerUser :: Reader -> UserId @-}

{-@ measure readerUserCap :: Entity Reader -> Bool @-}

{-@ assume readerUser' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == readerUser (entityVal row)},
                          {\field row -> field == readerUser (entityVal row)},
                          {\old -> readerUserCap old},
                          {\x_0 x_1 x_2 -> ((IsOwnerR x_2 x_0)) => (readerUserCap x_0)}>
                          (Entity User) Reader UserId
  @-}
readerUser' :: EntityFieldWrapper (Entity User) Reader UserId
readerUser' = EntityFieldWrapper ReaderUser

{-@ measure readerProject :: Reader -> ProjectId @-}

{-@ measure readerProjectCap :: Entity Reader -> Bool @-}

{-@ assume readerProject' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == readerProject (entityVal row)},
                          {\field row -> field == readerProject (entityVal row)},
                          {\old -> readerProjectCap old},
                          {\old _ _ -> readerProjectCap old}>
                          (Entity User) Reader ProjectId
  @-}
readerProject' :: EntityFieldWrapper (Entity User) Reader ProjectId
readerProject' = EntityFieldWrapper ReaderProject

-- * Collaborator
{-@ mkCollaborator ::
        x_0: UserId
     -> x_1: ProjectId
     -> StormRecord <{\row -> collaboratorUser (entityVal row) == x_0 && collaboratorProject (entityVal row) == x_1},
                     {\row user -> IsOwnerC user row},
                     {\x_0 x_1 -> False}>
                     (Entity User) Collaborator
  @-}
mkCollaborator :: UserId -> ProjectId -> StormRecord (Entity User) Collaborator
mkCollaborator x_0 x_1 = StormRecord (Collaborator x_0 x_1)

{-@ invariant {v: Entity Collaborator | v == getJust (entityKey v)} @-}

{-@ invariant {v: Entity Collaborator | isCollaborator (collaboratorUser (entityVal v)) (collaboratorProject (entityVal v))} @-}

{-@ assume collaboratorId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Collaborator CollaboratorId
  @-}
collaboratorId' :: EntityFieldWrapper (Entity User) Collaborator CollaboratorId
collaboratorId' = EntityFieldWrapper CollaboratorId

{-@ measure collaboratorUser :: Collaborator -> UserId @-}

{-@ measure collaboratorUserCap :: Entity Collaborator -> Bool @-}

{-@ assume collaboratorUser' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == collaboratorUser (entityVal row)},
                          {\field row -> field == collaboratorUser (entityVal row)},
                          {\old -> collaboratorUserCap old},
                          {\x_0 x_1 x_2 -> ((IsOwnerC x_2 x_0)) => (collaboratorUserCap x_0)}>
                          (Entity User) Collaborator UserId
  @-}
collaboratorUser' :: EntityFieldWrapper (Entity User) Collaborator UserId
collaboratorUser' = EntityFieldWrapper CollaboratorUser

{-@ measure collaboratorProject :: Collaborator -> ProjectId @-}

{-@ measure collaboratorProjectCap :: Entity Collaborator -> Bool @-}

{-@ assume collaboratorProject' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == collaboratorProject (entityVal row)},
                          {\field row -> field == collaboratorProject (entityVal row)},
                          {\old -> collaboratorProjectCap old},
                          {\old _ _ -> collaboratorProjectCap old}>
                          (Entity User) Collaborator ProjectId
  @-}
collaboratorProject' :: EntityFieldWrapper (Entity User) Collaborator ProjectId
collaboratorProject' = EntityFieldWrapper CollaboratorProject
