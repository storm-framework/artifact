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

module UrWeb.Calendar.Model
  ( migrateAll
  , mkUser
  , mkEvent
  , mkAttendee
  , mkTimeOnly
  , User
  , Event
  , Attendee
  , TimeOnly
  , userId'
  , userName'
  , userPass'
  , eventId'
  , eventCreator'
  , eventTime'
  , eventTitle'
  , eventDesc'
  , attendeeId'
  , attendeeEvent'
  , attendeeUser'
  , timeOnlyId'
  , timeOnlyEvent'
  , timeOnlyUser'
  , UserId
  , EventId
  , AttendeeId
  , TimeOnlyId
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
  

Event
  creator UserId
  time Int
  title String
  desc String
  

Attendee
  event EventId
  user UserId
  

TimeOnly
  event EventId
  user UserId
  
|]

--------------------------------------------------------------------------------
-- | Predicates
--------------------------------------------------------------------------------

{-@ measure isAttendee :: UserId -> EventId -> Bool @-}

{-@ measure hasTimeAccess :: UserId -> EventId -> Bool @-}

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

-- * Event
{-@ mkEvent ::
        x_0: UserId
     -> x_1: Int
     -> x_2: String
     -> x_3: String
     -> StormRecord <{\row -> eventCreator (entityVal row) == x_0 && eventTime (entityVal row) == x_1 && eventTitle (entityVal row) == x_2 && eventDesc (entityVal row) == x_3},
                     {\event viewer -> eventCreator (entityVal event) == entityKey viewer},
                     {\x_0 x_1 -> (eventCreator (entityVal x_0) == entityKey x_1 || isAttendee (entityKey x_1) (entityKey x_0)) || (hasTimeAccess (entityKey x_1) (entityKey x_0))}>
                     (Entity User) Event
  @-}
mkEvent :: UserId -> Int -> String -> String -> StormRecord (Entity User) Event
mkEvent x_0 x_1 x_2 x_3 = StormRecord (Event x_0 x_1 x_2 x_3)

{-@ invariant {v: Entity Event | v == getJust (entityKey v)} @-}



{-@ assume eventId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Event EventId
  @-}
eventId' :: EntityFieldWrapper (Entity User) Event EventId
eventId' = EntityFieldWrapper EventId

{-@ measure eventCreator :: Event -> UserId @-}

{-@ measure eventCreatorCap :: Entity Event -> Bool @-}

{-@ assume eventCreator' ::
      EntityFieldWrapper <{\x_0 x_1 -> (eventCreator (entityVal x_0) == entityKey x_1 || isAttendee (entityKey x_1) (entityKey x_0))},
                          {\row field -> field == eventCreator (entityVal row)},
                          {\field row -> field == eventCreator (entityVal row)},
                          {\old -> eventCreatorCap old},
                          {\x_0 x_1 x_2 -> ((eventCreator (entityVal x_0) == eventCreator (entityVal x_1) && eventCreator (entityVal x_0) == entityKey x_2)) => (eventCreatorCap x_0)}>
                          (Entity User) Event UserId
  @-}
eventCreator' :: EntityFieldWrapper (Entity User) Event UserId
eventCreator' = EntityFieldWrapper EventCreator

{-@ measure eventTime :: Event -> Int @-}

{-@ measure eventTimeCap :: Entity Event -> Bool @-}

{-@ assume eventTime' ::
      EntityFieldWrapper <{\x_0 x_1 -> (eventCreator (entityVal x_0) == entityKey x_1 || isAttendee (entityKey x_1) (entityKey x_0)) || (hasTimeAccess (entityKey x_1) (entityKey x_0))},
                          {\row field -> field == eventTime (entityVal row)},
                          {\field row -> field == eventTime (entityVal row)},
                          {\old -> eventTimeCap old},
                          {\x_0 x_1 x_2 -> ((eventCreator (entityVal x_0) == eventCreator (entityVal x_1) && eventCreator (entityVal x_0) == entityKey x_2)) => (eventTimeCap x_0)}>
                          (Entity User) Event Int
  @-}
eventTime' :: EntityFieldWrapper (Entity User) Event Int
eventTime' = EntityFieldWrapper EventTime

{-@ measure eventTitle :: Event -> String @-}

{-@ measure eventTitleCap :: Entity Event -> Bool @-}

{-@ assume eventTitle' ::
      EntityFieldWrapper <{\x_0 x_1 -> (eventCreator (entityVal x_0) == entityKey x_1 || isAttendee (entityKey x_1) (entityKey x_0))},
                          {\row field -> field == eventTitle (entityVal row)},
                          {\field row -> field == eventTitle (entityVal row)},
                          {\old -> eventTitleCap old},
                          {\x_0 x_1 x_2 -> ((eventCreator (entityVal x_0) == eventCreator (entityVal x_1) && eventCreator (entityVal x_0) == entityKey x_2)) => (eventTitleCap x_0)}>
                          (Entity User) Event String
  @-}
eventTitle' :: EntityFieldWrapper (Entity User) Event String
eventTitle' = EntityFieldWrapper EventTitle

{-@ measure eventDesc :: Event -> String @-}

{-@ measure eventDescCap :: Entity Event -> Bool @-}

{-@ assume eventDesc' ::
      EntityFieldWrapper <{\x_0 x_1 -> (eventCreator (entityVal x_0) == entityKey x_1 || isAttendee (entityKey x_1) (entityKey x_0))},
                          {\row field -> field == eventDesc (entityVal row)},
                          {\field row -> field == eventDesc (entityVal row)},
                          {\old -> eventDescCap old},
                          {\x_0 x_1 x_2 -> ((eventCreator (entityVal x_0) == eventCreator (entityVal x_1) && eventCreator (entityVal x_0) == entityKey x_2)) => (eventDescCap x_0)}>
                          (Entity User) Event String
  @-}
eventDesc' :: EntityFieldWrapper (Entity User) Event String
eventDesc' = EntityFieldWrapper EventDesc

-- * Attendee
{-@ mkAttendee ::
        x_0: EventId
     -> x_1: UserId
     -> StormRecord <{\row -> attendeeEvent (entityVal row) == x_0 && attendeeUser (entityVal row) == x_1},
                     {\row user -> entityKey user == eventCreator (entityVal (getJust (attendeeEvent (entityVal row))))},
                     {\x_0 x_1 -> False}>
                     (Entity User) Attendee
  @-}
mkAttendee :: EventId -> UserId -> StormRecord (Entity User) Attendee
mkAttendee x_0 x_1 = StormRecord (Attendee x_0 x_1)

{-@ invariant {v: Entity Attendee | v == getJust (entityKey v)} @-}

{-@ invariant {v: Entity Attendee | isAttendee (attendeeUser (entityVal v)) (attendeeEvent (entityVal v))} @-}

{-@ assume attendeeId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Attendee AttendeeId
  @-}
attendeeId' :: EntityFieldWrapper (Entity User) Attendee AttendeeId
attendeeId' = EntityFieldWrapper AttendeeId

{-@ measure attendeeEvent :: Attendee -> EventId @-}

{-@ measure attendeeEventCap :: Entity Attendee -> Bool @-}

{-@ assume attendeeEvent' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == attendeeEvent (entityVal row)},
                          {\field row -> field == attendeeEvent (entityVal row)},
                          {\old -> attendeeEventCap old},
                          {\x_0 x_1 x_2 -> ((attendeeEvent (entityVal x_0) == attendeeEvent (entityVal x_1) && entityKey x_2 == eventCreator (entityVal (getJust (attendeeEvent (entityVal x_0)))))) => (attendeeEventCap x_0)}>
                          (Entity User) Attendee EventId
  @-}
attendeeEvent' :: EntityFieldWrapper (Entity User) Attendee EventId
attendeeEvent' = EntityFieldWrapper AttendeeEvent

{-@ measure attendeeUser :: Attendee -> UserId @-}

{-@ measure attendeeUserCap :: Entity Attendee -> Bool @-}

{-@ assume attendeeUser' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == attendeeUser (entityVal row)},
                          {\field row -> field == attendeeUser (entityVal row)},
                          {\old -> attendeeUserCap old},
                          {\x_0 x_1 x_2 -> ((attendeeEvent (entityVal x_0) == attendeeEvent (entityVal x_1) && entityKey x_2 == eventCreator (entityVal (getJust (attendeeEvent (entityVal x_0)))))) => (attendeeUserCap x_0)}>
                          (Entity User) Attendee UserId
  @-}
attendeeUser' :: EntityFieldWrapper (Entity User) Attendee UserId
attendeeUser' = EntityFieldWrapper AttendeeUser

-- * TimeOnly
{-@ mkTimeOnly ::
        x_0: EventId
     -> x_1: UserId
     -> StormRecord <{\row -> timeOnlyEvent (entityVal row) == x_0 && timeOnlyUser (entityVal row) == x_1},
                     {\row user -> entityKey user == eventCreator (entityVal (getJust (timeOnlyEvent (entityVal row))))},
                     {\x_0 x_1 -> False}>
                     (Entity User) TimeOnly
  @-}
mkTimeOnly :: EventId -> UserId -> StormRecord (Entity User) TimeOnly
mkTimeOnly x_0 x_1 = StormRecord (TimeOnly x_0 x_1)

{-@ invariant {v: Entity TimeOnly | v == getJust (entityKey v)} @-}

{-@ invariant {v: Entity TimeOnly | hasTimeAccess (timeOnlyUser (entityVal v)) (timeOnlyEvent (entityVal v))} @-}

{-@ assume timeOnlyId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) TimeOnly TimeOnlyId
  @-}
timeOnlyId' :: EntityFieldWrapper (Entity User) TimeOnly TimeOnlyId
timeOnlyId' = EntityFieldWrapper TimeOnlyId

{-@ measure timeOnlyEvent :: TimeOnly -> EventId @-}

{-@ measure timeOnlyEventCap :: Entity TimeOnly -> Bool @-}

{-@ assume timeOnlyEvent' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == timeOnlyEvent (entityVal row)},
                          {\field row -> field == timeOnlyEvent (entityVal row)},
                          {\old -> timeOnlyEventCap old},
                          {\x_0 x_1 x_2 -> ((timeOnlyEvent (entityVal x_0) == timeOnlyEvent (entityVal x_1) && entityKey x_2 == eventCreator (entityVal (getJust (timeOnlyEvent (entityVal x_0)))))) => (timeOnlyEventCap x_0)}>
                          (Entity User) TimeOnly EventId
  @-}
timeOnlyEvent' :: EntityFieldWrapper (Entity User) TimeOnly EventId
timeOnlyEvent' = EntityFieldWrapper TimeOnlyEvent

{-@ measure timeOnlyUser :: TimeOnly -> UserId @-}

{-@ measure timeOnlyUserCap :: Entity TimeOnly -> Bool @-}

{-@ assume timeOnlyUser' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == timeOnlyUser (entityVal row)},
                          {\field row -> field == timeOnlyUser (entityVal row)},
                          {\old -> timeOnlyUserCap old},
                          {\x_0 x_1 x_2 -> ((timeOnlyEvent (entityVal x_0) == timeOnlyEvent (entityVal x_1) && entityKey x_2 == eventCreator (entityVal (getJust (timeOnlyEvent (entityVal x_0)))))) => (timeOnlyUserCap x_0)}>
                          (Entity User) TimeOnly UserId
  @-}
timeOnlyUser' :: EntityFieldWrapper (Entity User) TimeOnly UserId
timeOnlyUser' = EntityFieldWrapper TimeOnlyUser
