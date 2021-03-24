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

module UrWeb.Poll.Model
  ( migrateAll
  , mkUser
  , mkPoll
  , mkQuestion
  , mkAnswer
  , User
  , Poll
  , Question
  , Answer
  , userId'
  , userCode'
  , pollId'
  , pollName'
  , pollOwner'
  , pollLive'
  , questionId'
  , questionPoll'
  , questionText'
  , answerId'
  , answerPoll'
  , answerOwner'
  , answerText'
  , UserId
  , PollId
  , QuestionId
  , AnswerId
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
  code String
  

Poll
  name String
  owner UserId
  live Bool
  

Question
  poll PollId
  text String
  

Answer
  poll PollId
  owner UserId
  text String
  
|]

--------------------------------------------------------------------------------
-- | Predicates
--------------------------------------------------------------------------------

{-@ measure isLive :: PollId -> Bool @-}

{-@ measure isPollOwner :: UserId -> PollId -> Bool @-}

--------------------------------------------------------------------------------
-- | Policies
--------------------------------------------------------------------------------

{-@ predicate IsLiveQ ROW = isLive (questionPoll (entityVal ROW)) @-}

{-@ predicate IsPollOwnerQ USER ROW = isPollOwner (entityKey USER) (questionPoll (entityVal ROW)) @-}

{-@ predicate IsLiveA ROW = isLive (answerPoll (entityVal ROW)) @-}

{-@ predicate IsPollOwnerA USER ROW = isPollOwner (entityKey USER) (answerPoll (entityVal ROW)) @-}

--------------------------------------------------------------------------------
-- | Records
--------------------------------------------------------------------------------

{-@ measure getJust :: Key record -> Entity record @-}

-- * User
{-@ mkUser ::
        x_0: String
     -> StormRecord <{\row -> userCode (entityVal row) == x_0},
                     {\_ _ -> True},
                     {\x_0 x_1 -> (x_0 == x_1)}>
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

{-@ measure userCode :: User -> String @-}

{-@ measure userCodeCap :: Entity User -> Bool @-}

{-@ assume userCode' ::
      EntityFieldWrapper <{\x_0 x_1 -> (x_0 == x_1)},
                          {\row field -> field == userCode (entityVal row)},
                          {\field row -> field == userCode (entityVal row)},
                          {\old -> userCodeCap old},
                          {\old _ _ -> userCodeCap old}>
                          (Entity User) User String
  @-}
userCode' :: EntityFieldWrapper (Entity User) User String
userCode' = EntityFieldWrapper UserCode

-- * Poll
{-@ mkPoll ::
        x_0: String
     -> x_1: UserId
     -> x_2: Bool
     -> StormRecord <{\row -> pollName (entityVal row) == x_0 && pollOwner (entityVal row) == x_1 && pollLive (entityVal row) == x_2},
                     {\row user -> not (pollLive (entityVal row)) && pollOwner (entityVal row) == entityKey user},
                     {\x_0 x_1 -> (pollLive (entityVal x_0) || pollOwner (entityVal x_0) == entityKey x_1)}>
                     (Entity User) Poll
  @-}
mkPoll :: String -> UserId -> Bool -> StormRecord (Entity User) Poll
mkPoll x_0 x_1 x_2 = StormRecord (Poll x_0 x_1 x_2)

{-@ invariant {v: Entity Poll | v == getJust (entityKey v)} @-}

{-@ invariant {v: Entity Poll | (pollLive (entityVal v)) => isLive (entityKey v)} @-}

{-@ assume pollId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Poll PollId
  @-}
pollId' :: EntityFieldWrapper (Entity User) Poll PollId
pollId' = EntityFieldWrapper PollId

{-@ measure pollName :: Poll -> String @-}

{-@ measure pollNameCap :: Entity Poll -> Bool @-}

{-@ assume pollName' ::
      EntityFieldWrapper <{\x_0 x_1 -> (pollLive (entityVal x_0) || pollOwner (entityVal x_0) == entityKey x_1)},
                          {\row field -> field == pollName (entityVal row)},
                          {\field row -> field == pollName (entityVal row)},
                          {\old -> pollNameCap old},
                          {\old _ _ -> pollNameCap old}>
                          (Entity User) Poll String
  @-}
pollName' :: EntityFieldWrapper (Entity User) Poll String
pollName' = EntityFieldWrapper PollName

{-@ measure pollOwner :: Poll -> UserId @-}

{-@ measure pollOwnerCap :: Entity Poll -> Bool @-}

{-@ assume pollOwner' ::
      EntityFieldWrapper <{\x_0 x_1 -> (pollLive (entityVal x_0) || pollOwner (entityVal x_0) == entityKey x_1)},
                          {\row field -> field == pollOwner (entityVal row)},
                          {\field row -> field == pollOwner (entityVal row)},
                          {\old -> pollOwnerCap old},
                          {\old _ _ -> pollOwnerCap old}>
                          (Entity User) Poll UserId
  @-}
pollOwner' :: EntityFieldWrapper (Entity User) Poll UserId
pollOwner' = EntityFieldWrapper PollOwner

{-@ measure pollLive :: Poll -> Bool @-}

{-@ measure pollLiveCap :: Entity Poll -> Bool @-}

{-@ assume pollLive' ::
      EntityFieldWrapper <{\x_0 x_1 -> (pollLive (entityVal x_0) || pollOwner (entityVal x_0) == entityKey x_1)},
                          {\row field -> field == pollLive (entityVal row)},
                          {\field row -> field == pollLive (entityVal row)},
                          {\old -> pollLiveCap old},
                          {\x_0 x_1 x_2 -> ((pollOwner (entityVal x_0) == entityKey x_2 && pollLive (entityVal x_1))) => (pollLiveCap x_0)}>
                          (Entity User) Poll Bool
  @-}
pollLive' :: EntityFieldWrapper (Entity User) Poll Bool
pollLive' = EntityFieldWrapper PollLive

-- * Question
{-@ mkQuestion ::
        x_0: PollId
     -> x_1: String
     -> StormRecord <{\row -> questionPoll (entityVal row) == x_0 && questionText (entityVal row) == x_1},
                     {\row user -> not (IsLiveQ row) && IsPollOwnerQ user row},
                     {\x_0 x_1 -> (IsPollOwnerQ x_1 x_0 || IsLiveQ x_0)}>
                     (Entity User) Question
  @-}
mkQuestion :: PollId -> String -> StormRecord (Entity User) Question
mkQuestion x_0 x_1 = StormRecord (Question x_0 x_1)

{-@ invariant {v: Entity Question | v == getJust (entityKey v)} @-}



{-@ assume questionId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Question QuestionId
  @-}
questionId' :: EntityFieldWrapper (Entity User) Question QuestionId
questionId' = EntityFieldWrapper QuestionId

{-@ measure questionPoll :: Question -> PollId @-}

{-@ measure questionPollCap :: Entity Question -> Bool @-}

{-@ assume questionPoll' ::
      EntityFieldWrapper <{\x_0 x_1 -> (IsPollOwnerQ x_1 x_0 || IsLiveQ x_0)},
                          {\row field -> field == questionPoll (entityVal row)},
                          {\field row -> field == questionPoll (entityVal row)},
                          {\old -> questionPollCap old},
                          {\old _ _ -> questionPollCap old}>
                          (Entity User) Question PollId
  @-}
questionPoll' :: EntityFieldWrapper (Entity User) Question PollId
questionPoll' = EntityFieldWrapper QuestionPoll

{-@ measure questionText :: Question -> String @-}

{-@ measure questionTextCap :: Entity Question -> Bool @-}

{-@ assume questionText' ::
      EntityFieldWrapper <{\x_0 x_1 -> (IsPollOwnerQ x_1 x_0 || IsLiveQ x_0)},
                          {\row field -> field == questionText (entityVal row)},
                          {\field row -> field == questionText (entityVal row)},
                          {\old -> questionTextCap old},
                          {\old _ _ -> questionTextCap old}>
                          (Entity User) Question String
  @-}
questionText' :: EntityFieldWrapper (Entity User) Question String
questionText' = EntityFieldWrapper QuestionText

-- * Answer
{-@ mkAnswer ::
        x_0: PollId
     -> x_1: UserId
     -> x_2: String
     -> StormRecord <{\row -> answerPoll (entityVal row) == x_0 && answerOwner (entityVal row) == x_1 && answerText (entityVal row) == x_2},
                     {\row user -> answerOwner (entityVal row) == entityKey user && IsLiveA row},
                     {\x_0 x_1 -> (IsPollOwnerA x_1 x_0) || (answerOwner (entityVal x_0) == entityKey x_1)}>
                     (Entity User) Answer
  @-}
mkAnswer :: PollId -> UserId -> String -> StormRecord (Entity User) Answer
mkAnswer x_0 x_1 x_2 = StormRecord (Answer x_0 x_1 x_2)

{-@ invariant {v: Entity Answer | v == getJust (entityKey v)} @-}



{-@ assume answerId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Answer AnswerId
  @-}
answerId' :: EntityFieldWrapper (Entity User) Answer AnswerId
answerId' = EntityFieldWrapper AnswerId

{-@ measure answerPoll :: Answer -> PollId @-}

{-@ measure answerPollCap :: Entity Answer -> Bool @-}

{-@ assume answerPoll' ::
      EntityFieldWrapper <{\x_0 x_1 -> (IsPollOwnerA x_1 x_0) || (answerOwner (entityVal x_0) == entityKey x_1)},
                          {\row field -> field == answerPoll (entityVal row)},
                          {\field row -> field == answerPoll (entityVal row)},
                          {\old -> answerPollCap old},
                          {\old _ _ -> answerPollCap old}>
                          (Entity User) Answer PollId
  @-}
answerPoll' :: EntityFieldWrapper (Entity User) Answer PollId
answerPoll' = EntityFieldWrapper AnswerPoll

{-@ measure answerOwner :: Answer -> UserId @-}

{-@ measure answerOwnerCap :: Entity Answer -> Bool @-}

{-@ assume answerOwner' ::
      EntityFieldWrapper <{\x_0 x_1 -> (IsPollOwnerA x_1 x_0) || (answerOwner (entityVal x_0) == entityKey x_1)},
                          {\row field -> field == answerOwner (entityVal row)},
                          {\field row -> field == answerOwner (entityVal row)},
                          {\old -> answerOwnerCap old},
                          {\old _ _ -> answerOwnerCap old}>
                          (Entity User) Answer UserId
  @-}
answerOwner' :: EntityFieldWrapper (Entity User) Answer UserId
answerOwner' = EntityFieldWrapper AnswerOwner

{-@ measure answerText :: Answer -> String @-}

{-@ measure answerTextCap :: Entity Answer -> Bool @-}

{-@ assume answerText' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == answerText (entityVal row)},
                          {\field row -> field == answerText (entityVal row)},
                          {\old -> answerTextCap old},
                          {\old _ _ -> answerTextCap old}>
                          (Entity User) Answer String
  @-}
answerText' :: EntityFieldWrapper (Entity User) Answer String
answerText' = EntityFieldWrapper AnswerText
