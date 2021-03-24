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
  , mkPaper
  , mkReviewAssignment
  , mkReview
  , mkPaperCoauthor
  , User
  , Paper
  , ReviewAssignment
  , Review
  , PaperCoauthor
  , userId'
  , userUsername'
  , userName'
  , userEmail'
  , userAffiliation'
  , userLevel'
  , paperId'
  , paperAuthor'
  , paperTitle'
  , paperAbstract'
  , paperAccepted'
  , reviewAssignmentId'
  , reviewAssignmentPaper'
  , reviewAssignmentUser'
  , reviewAssignmentAssignType'
  , reviewId'
  , reviewPaper'
  , reviewReviewer'
  , reviewContent'
  , reviewScore'
  , paperCoauthorId'
  , paperCoauthorPaper'
  , paperCoauthorAuthor'
  , UserId
  , PaperId
  , ReviewAssignmentId
  , ReviewId
  , PaperCoauthorId
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

import Data.Text (Text)
import Stage

--------------------------------------------------------------------------------
-- | Inline
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- | Persistent
--------------------------------------------------------------------------------

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  username Text
  name Text
  email Text
  affiliation Text
  level String
  

Paper
  author UserId
  title Text
  abstract Text
  accepted Bool
  

ReviewAssignment
  paper PaperId
  user UserId
  assignType Text
  

Review
  paper PaperId
  reviewer UserId
  content Text
  score Int
  

PaperCoauthor
  paper PaperId
  author Text
  
|]

--------------------------------------------------------------------------------
-- | Predicates
--------------------------------------------------------------------------------

{-@ measure isAuthor :: UserId -> PaperId -> Bool @-}

{-@ measure isAccepted :: PaperId -> Bool @-}

{-@ measure isReviewer :: UserId -> PaperId -> Bool @-}

--------------------------------------------------------------------------------
-- | Policies
--------------------------------------------------------------------------------

{-@ predicate IsChair USER = userLevel (entityVal USER) == "chair" @-}

{-@ predicate IsPc USER = userLevel (entityVal USER) == "pc" || userLevel (entityVal USER) == "chair" @-}

{-@ predicate ChairOrSelf USER VIEWER = IsChair VIEWER || entityKey VIEWER == entityKey USER @-}

{-@ predicate PcOrPublic ROW VIEWER = IsPc VIEWER || currentStage == "public" @-}

{-@ predicate PcOrAuthorOrAccepted PAPER VIEWER = IsPc VIEWER || isAuthor (entityKey VIEWER) (entityKey PAPER) || (currentStage == "public" && paperAccepted (entityVal PAPER)) @-}

{-@ predicate PcOrAuthorOrAccepted' COAUTHOR VIEWER = IsPc VIEWER || isAuthor (entityKey VIEWER) (paperCoauthorPaper (entityVal COAUTHOR)) || (currentStage == "public" && isAccepted (paperCoauthorPaper (entityVal COAUTHOR))) @-}

{-@ predicate OnlyPc ROW VIEWER = IsPc VIEWER @-}

{-@ predicate PcOrAuthorIfPublic REVIEW VIEWER = IsPc VIEWER || (currentStage == "public" && isAuthor (entityKey VIEWER) (reviewPaper (entityVal REVIEW))) @-}

--------------------------------------------------------------------------------
-- | Records
--------------------------------------------------------------------------------

{-@ measure getJust :: Key record -> Entity record @-}

-- * User
{-@ mkUser ::
        x_0: Text
     -> x_1: Text
     -> x_2: Text
     -> x_3: Text
     -> x_4: String
     -> StormRecord <{\row -> userUsername (entityVal row) == x_0 && userName (entityVal row) == x_1 && userEmail (entityVal row) == x_2 && userAffiliation (entityVal row) == x_3 && userLevel (entityVal row) == x_4},
                     {\_ _ -> True},
                     {\x_0 x_1 -> (IsChair x_1 || entityKey x_1 == entityKey x_0)}>
                     (Entity User) User
  @-}
mkUser :: Text -> Text -> Text -> Text -> String -> StormRecord (Entity User) User
mkUser x_0 x_1 x_2 x_3 x_4 = StormRecord (User x_0 x_1 x_2 x_3 x_4)

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

{-@ measure userUsername :: User -> Text @-}

{-@ measure userUsernameCap :: Entity User -> Bool @-}

{-@ assume userUsername' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userUsername (entityVal row)},
                          {\field row -> field == userUsername (entityVal row)},
                          {\old -> userUsernameCap old},
                          {\old _ _ -> userUsernameCap old}>
                          (Entity User) User Text
  @-}
userUsername' :: EntityFieldWrapper (Entity User) User Text
userUsername' = EntityFieldWrapper UserUsername

{-@ measure userName :: User -> Text @-}

{-@ measure userNameCap :: Entity User -> Bool @-}

{-@ assume userName' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userName (entityVal row)},
                          {\field row -> field == userName (entityVal row)},
                          {\old -> userNameCap old},
                          {\old _ _ -> userNameCap old}>
                          (Entity User) User Text
  @-}
userName' :: EntityFieldWrapper (Entity User) User Text
userName' = EntityFieldWrapper UserName

{-@ measure userEmail :: User -> Text @-}

{-@ measure userEmailCap :: Entity User -> Bool @-}

{-@ assume userEmail' ::
      EntityFieldWrapper <{\x_0 x_1 -> (IsChair x_1 || entityKey x_1 == entityKey x_0)},
                          {\row field -> field == userEmail (entityVal row)},
                          {\field row -> field == userEmail (entityVal row)},
                          {\old -> userEmailCap old},
                          {\old _ _ -> userEmailCap old}>
                          (Entity User) User Text
  @-}
userEmail' :: EntityFieldWrapper (Entity User) User Text
userEmail' = EntityFieldWrapper UserEmail

{-@ measure userAffiliation :: User -> Text @-}

{-@ measure userAffiliationCap :: Entity User -> Bool @-}

{-@ assume userAffiliation' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userAffiliation (entityVal row)},
                          {\field row -> field == userAffiliation (entityVal row)},
                          {\old -> userAffiliationCap old},
                          {\old _ _ -> userAffiliationCap old}>
                          (Entity User) User Text
  @-}
userAffiliation' :: EntityFieldWrapper (Entity User) User Text
userAffiliation' = EntityFieldWrapper UserAffiliation

{-@ measure userLevel :: User -> String @-}

{-@ measure userLevelCap :: Entity User -> Bool @-}

{-@ assume userLevel' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userLevel (entityVal row)},
                          {\field row -> field == userLevel (entityVal row)},
                          {\old -> userLevelCap old},
                          {\old _ _ -> userLevelCap old}>
                          (Entity User) User String
  @-}
userLevel' :: EntityFieldWrapper (Entity User) User String
userLevel' = EntityFieldWrapper UserLevel

-- * Paper
{-@ mkPaper ::
        x_0: UserId
     -> x_1: Text
     -> x_2: Text
     -> x_3: Bool
     -> StormRecord <{\row -> paperAuthor (entityVal row) == x_0 && paperTitle (entityVal row) == x_1 && paperAbstract (entityVal row) == x_2 && paperAccepted (entityVal row) == x_3},
                     {\paper viewer -> paperAuthor (entityVal paper) == entityKey viewer && paperAccepted (entityVal paper) == False && currentStage == "submit"},
                     {\x_0 x_1 -> (IsPc x_1 || isAuthor (entityKey x_1) (entityKey x_0) || (currentStage == "public" && paperAccepted (entityVal x_0))) || (IsPc x_1 || currentStage == "public")}>
                     (Entity User) Paper
  @-}
mkPaper :: UserId -> Text -> Text -> Bool -> StormRecord (Entity User) Paper
mkPaper x_0 x_1 x_2 x_3 = StormRecord (Paper x_0 x_1 x_2 x_3)

{-@ invariant {v: Entity Paper | v == getJust (entityKey v)} @-}

{-@ invariant {v: Entity Paper | isAuthor (paperAuthor (entityVal v)) (entityKey v)} @-}

{-@ invariant {v: Entity Paper | (paperAccepted (entityVal v)) => isAccepted (entityKey v)} @-}

{-@ assume paperId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Paper PaperId
  @-}
paperId' :: EntityFieldWrapper (Entity User) Paper PaperId
paperId' = EntityFieldWrapper PaperId

{-@ measure paperAuthor :: Paper -> UserId @-}

{-@ measure paperAuthorCap :: Entity Paper -> Bool @-}

{-@ assume paperAuthor' ::
      EntityFieldWrapper <{\x_0 x_1 -> (IsPc x_1 || isAuthor (entityKey x_1) (entityKey x_0) || (currentStage == "public" && paperAccepted (entityVal x_0)))},
                          {\row field -> field == paperAuthor (entityVal row)},
                          {\field row -> field == paperAuthor (entityVal row)},
                          {\old -> paperAuthorCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (paperAuthorCap x_0)}>
                          (Entity User) Paper UserId
  @-}
paperAuthor' :: EntityFieldWrapper (Entity User) Paper UserId
paperAuthor' = EntityFieldWrapper PaperAuthor

{-@ measure paperTitle :: Paper -> Text @-}

{-@ measure paperTitleCap :: Entity Paper -> Bool @-}

{-@ assume paperTitle' ::
      EntityFieldWrapper <{\x_0 x_1 -> (IsPc x_1 || isAuthor (entityKey x_1) (entityKey x_0) || (currentStage == "public" && paperAccepted (entityVal x_0)))},
                          {\row field -> field == paperTitle (entityVal row)},
                          {\field row -> field == paperTitle (entityVal row)},
                          {\old -> paperTitleCap old},
                          {\x_0 x_1 x_2 -> ((entityKey x_2 == paperAuthor (entityVal x_0) && currentStage == "submit")) => (paperTitleCap x_0)}>
                          (Entity User) Paper Text
  @-}
paperTitle' :: EntityFieldWrapper (Entity User) Paper Text
paperTitle' = EntityFieldWrapper PaperTitle

{-@ measure paperAbstract :: Paper -> Text @-}

{-@ measure paperAbstractCap :: Entity Paper -> Bool @-}

{-@ assume paperAbstract' ::
      EntityFieldWrapper <{\x_0 x_1 -> (IsPc x_1 || isAuthor (entityKey x_1) (entityKey x_0) || (currentStage == "public" && paperAccepted (entityVal x_0)))},
                          {\row field -> field == paperAbstract (entityVal row)},
                          {\field row -> field == paperAbstract (entityVal row)},
                          {\old -> paperAbstractCap old},
                          {\x_0 x_1 x_2 -> ((entityKey x_2 == paperAuthor (entityVal x_0) && currentStage == "submit")) => (paperAbstractCap x_0)}>
                          (Entity User) Paper Text
  @-}
paperAbstract' :: EntityFieldWrapper (Entity User) Paper Text
paperAbstract' = EntityFieldWrapper PaperAbstract

{-@ measure paperAccepted :: Paper -> Bool @-}

{-@ measure paperAcceptedCap :: Entity Paper -> Bool @-}

{-@ assume paperAccepted' ::
      EntityFieldWrapper <{\x_0 x_1 -> (IsPc x_1 || currentStage == "public")},
                          {\row field -> field == paperAccepted (entityVal row)},
                          {\field row -> field == paperAccepted (entityVal row)},
                          {\old -> paperAcceptedCap old},
                          {\x_0 x_1 x_2 -> ((IsChair x_2 && currentStage == "review")) => (paperAcceptedCap x_0)}>
                          (Entity User) Paper Bool
  @-}
paperAccepted' :: EntityFieldWrapper (Entity User) Paper Bool
paperAccepted' = EntityFieldWrapper PaperAccepted

-- * ReviewAssignment
{-@ mkReviewAssignment ::
        x_0: PaperId
     -> x_1: UserId
     -> x_2: Text
     -> StormRecord <{\row -> reviewAssignmentPaper (entityVal row) == x_0 && reviewAssignmentUser (entityVal row) == x_1 && reviewAssignmentAssignType (entityVal row) == x_2},
                     {\row viewer -> IsChair viewer && IsPc (getJust (reviewAssignmentUser (entityVal row))) && currentStage == "review"},
                     {\x_0 x_1 -> (IsPc x_1)}>
                     (Entity User) ReviewAssignment
  @-}
mkReviewAssignment :: PaperId -> UserId -> Text -> StormRecord (Entity User) ReviewAssignment
mkReviewAssignment x_0 x_1 x_2 = StormRecord (ReviewAssignment x_0 x_1 x_2)

{-@ invariant {v: Entity ReviewAssignment | v == getJust (entityKey v)} @-}

{-@ invariant {v: Entity ReviewAssignment | isReviewer (reviewAssignmentUser (entityVal v)) (reviewAssignmentPaper (entityVal v))} @-}

{-@ assume reviewAssignmentId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) ReviewAssignment ReviewAssignmentId
  @-}
reviewAssignmentId' :: EntityFieldWrapper (Entity User) ReviewAssignment ReviewAssignmentId
reviewAssignmentId' = EntityFieldWrapper ReviewAssignmentId

{-@ measure reviewAssignmentPaper :: ReviewAssignment -> PaperId @-}

{-@ measure reviewAssignmentPaperCap :: Entity ReviewAssignment -> Bool @-}

{-@ assume reviewAssignmentPaper' ::
      EntityFieldWrapper <{\x_0 x_1 -> (IsPc x_1)},
                          {\row field -> field == reviewAssignmentPaper (entityVal row)},
                          {\field row -> field == reviewAssignmentPaper (entityVal row)},
                          {\old -> reviewAssignmentPaperCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (reviewAssignmentPaperCap x_0)}>
                          (Entity User) ReviewAssignment PaperId
  @-}
reviewAssignmentPaper' :: EntityFieldWrapper (Entity User) ReviewAssignment PaperId
reviewAssignmentPaper' = EntityFieldWrapper ReviewAssignmentPaper

{-@ measure reviewAssignmentUser :: ReviewAssignment -> UserId @-}

{-@ measure reviewAssignmentUserCap :: Entity ReviewAssignment -> Bool @-}

{-@ assume reviewAssignmentUser' ::
      EntityFieldWrapper <{\x_0 x_1 -> (IsPc x_1)},
                          {\row field -> field == reviewAssignmentUser (entityVal row)},
                          {\field row -> field == reviewAssignmentUser (entityVal row)},
                          {\old -> reviewAssignmentUserCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (reviewAssignmentUserCap x_0)}>
                          (Entity User) ReviewAssignment UserId
  @-}
reviewAssignmentUser' :: EntityFieldWrapper (Entity User) ReviewAssignment UserId
reviewAssignmentUser' = EntityFieldWrapper ReviewAssignmentUser

{-@ measure reviewAssignmentAssignType :: ReviewAssignment -> Text @-}

{-@ measure reviewAssignmentAssignTypeCap :: Entity ReviewAssignment -> Bool @-}

{-@ assume reviewAssignmentAssignType' ::
      EntityFieldWrapper <{\x_0 x_1 -> (IsPc x_1)},
                          {\row field -> field == reviewAssignmentAssignType (entityVal row)},
                          {\field row -> field == reviewAssignmentAssignType (entityVal row)},
                          {\old -> reviewAssignmentAssignTypeCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (reviewAssignmentAssignTypeCap x_0)}>
                          (Entity User) ReviewAssignment Text
  @-}
reviewAssignmentAssignType' :: EntityFieldWrapper (Entity User) ReviewAssignment Text
reviewAssignmentAssignType' = EntityFieldWrapper ReviewAssignmentAssignType

-- * Review
{-@ mkReview ::
        x_0: PaperId
     -> x_1: UserId
     -> x_2: Text
     -> x_3: Int
     -> StormRecord <{\row -> reviewPaper (entityVal row) == x_0 && reviewReviewer (entityVal row) == x_1 && reviewContent (entityVal row) == x_2 && reviewScore (entityVal row) == x_3},
                     {\review viewer -> currentStage == "review" && reviewReviewer (entityVal review) == entityKey viewer && isReviewer (entityKey viewer) (reviewPaper (entityVal review))},
                     {\x_0 x_1 -> (IsPc x_1 || (currentStage == "public" && isAuthor (entityKey x_1) (reviewPaper (entityVal x_0)))) || (IsPc x_1)}>
                     (Entity User) Review
  @-}
mkReview :: PaperId -> UserId -> Text -> Int -> StormRecord (Entity User) Review
mkReview x_0 x_1 x_2 x_3 = StormRecord (Review x_0 x_1 x_2 x_3)

{-@ invariant {v: Entity Review | v == getJust (entityKey v)} @-}



{-@ assume reviewId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Review ReviewId
  @-}
reviewId' :: EntityFieldWrapper (Entity User) Review ReviewId
reviewId' = EntityFieldWrapper ReviewId

{-@ measure reviewPaper :: Review -> PaperId @-}

{-@ measure reviewPaperCap :: Entity Review -> Bool @-}

{-@ assume reviewPaper' ::
      EntityFieldWrapper <{\x_0 x_1 -> (IsPc x_1 || (currentStage == "public" && isAuthor (entityKey x_1) (reviewPaper (entityVal x_0))))},
                          {\row field -> field == reviewPaper (entityVal row)},
                          {\field row -> field == reviewPaper (entityVal row)},
                          {\old -> reviewPaperCap old},
                          {\x_0 x_1 x_2 -> ((isReviewer (entityKey x_2) (reviewPaper (entityVal x_0)) && currentStage == "review")) => (reviewPaperCap x_0)}>
                          (Entity User) Review PaperId
  @-}
reviewPaper' :: EntityFieldWrapper (Entity User) Review PaperId
reviewPaper' = EntityFieldWrapper ReviewPaper

{-@ measure reviewReviewer :: Review -> UserId @-}

{-@ measure reviewReviewerCap :: Entity Review -> Bool @-}

{-@ assume reviewReviewer' ::
      EntityFieldWrapper <{\x_0 x_1 -> (IsPc x_1)},
                          {\row field -> field == reviewReviewer (entityVal row)},
                          {\field row -> field == reviewReviewer (entityVal row)},
                          {\old -> reviewReviewerCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (reviewReviewerCap x_0)}>
                          (Entity User) Review UserId
  @-}
reviewReviewer' :: EntityFieldWrapper (Entity User) Review UserId
reviewReviewer' = EntityFieldWrapper ReviewReviewer

{-@ measure reviewContent :: Review -> Text @-}

{-@ measure reviewContentCap :: Entity Review -> Bool @-}

{-@ assume reviewContent' ::
      EntityFieldWrapper <{\x_0 x_1 -> (IsPc x_1 || (currentStage == "public" && isAuthor (entityKey x_1) (reviewPaper (entityVal x_0))))},
                          {\row field -> field == reviewContent (entityVal row)},
                          {\field row -> field == reviewContent (entityVal row)},
                          {\old -> reviewContentCap old},
                          {\x_0 x_1 x_2 -> ((isReviewer (entityKey x_2) (reviewPaper (entityVal x_0)) && currentStage == "review")) => (reviewContentCap x_0)}>
                          (Entity User) Review Text
  @-}
reviewContent' :: EntityFieldWrapper (Entity User) Review Text
reviewContent' = EntityFieldWrapper ReviewContent

{-@ measure reviewScore :: Review -> Int @-}

{-@ measure reviewScoreCap :: Entity Review -> Bool @-}

{-@ assume reviewScore' ::
      EntityFieldWrapper <{\x_0 x_1 -> (IsPc x_1 || (currentStage == "public" && isAuthor (entityKey x_1) (reviewPaper (entityVal x_0))))},
                          {\row field -> field == reviewScore (entityVal row)},
                          {\field row -> field == reviewScore (entityVal row)},
                          {\old -> reviewScoreCap old},
                          {\x_0 x_1 x_2 -> ((isReviewer (entityKey x_2) (reviewPaper (entityVal x_0)) && currentStage == "review")) => (reviewScoreCap x_0)}>
                          (Entity User) Review Int
  @-}
reviewScore' :: EntityFieldWrapper (Entity User) Review Int
reviewScore' = EntityFieldWrapper ReviewScore

-- * PaperCoauthor
{-@ mkPaperCoauthor ::
        x_0: PaperId
     -> x_1: Text
     -> StormRecord <{\row -> paperCoauthorPaper (entityVal row) == x_0 && paperCoauthorAuthor (entityVal row) == x_1},
                     {\_ _ -> True},
                     {\x_0 x_1 -> (IsPc x_1 || isAuthor (entityKey x_1) (paperCoauthorPaper (entityVal x_0)) || (currentStage == "public" && isAccepted (paperCoauthorPaper (entityVal x_0))))}>
                     (Entity User) PaperCoauthor
  @-}
mkPaperCoauthor :: PaperId -> Text -> StormRecord (Entity User) PaperCoauthor
mkPaperCoauthor x_0 x_1 = StormRecord (PaperCoauthor x_0 x_1)

{-@ invariant {v: Entity PaperCoauthor | v == getJust (entityKey v)} @-}



{-@ assume paperCoauthorId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) PaperCoauthor PaperCoauthorId
  @-}
paperCoauthorId' :: EntityFieldWrapper (Entity User) PaperCoauthor PaperCoauthorId
paperCoauthorId' = EntityFieldWrapper PaperCoauthorId

{-@ measure paperCoauthorPaper :: PaperCoauthor -> PaperId @-}

{-@ measure paperCoauthorPaperCap :: Entity PaperCoauthor -> Bool @-}

{-@ assume paperCoauthorPaper' ::
      EntityFieldWrapper <{\x_0 x_1 -> (IsPc x_1 || isAuthor (entityKey x_1) (paperCoauthorPaper (entityVal x_0)) || (currentStage == "public" && isAccepted (paperCoauthorPaper (entityVal x_0))))},
                          {\row field -> field == paperCoauthorPaper (entityVal row)},
                          {\field row -> field == paperCoauthorPaper (entityVal row)},
                          {\old -> paperCoauthorPaperCap old},
                          {\old _ _ -> paperCoauthorPaperCap old}>
                          (Entity User) PaperCoauthor PaperId
  @-}
paperCoauthorPaper' :: EntityFieldWrapper (Entity User) PaperCoauthor PaperId
paperCoauthorPaper' = EntityFieldWrapper PaperCoauthorPaper

{-@ measure paperCoauthorAuthor :: PaperCoauthor -> Text @-}

{-@ measure paperCoauthorAuthorCap :: Entity PaperCoauthor -> Bool @-}

{-@ assume paperCoauthorAuthor' ::
      EntityFieldWrapper <{\x_0 x_1 -> (IsPc x_1 || isAuthor (entityKey x_1) (paperCoauthorPaper (entityVal x_0)) || (currentStage == "public" && isAccepted (paperCoauthorPaper (entityVal x_0))))},
                          {\row field -> field == paperCoauthorAuthor (entityVal row)},
                          {\field row -> field == paperCoauthorAuthor (entityVal row)},
                          {\old -> paperCoauthorAuthorCap old},
                          {\old _ _ -> paperCoauthorAuthorCap old}>
                          (Entity User) PaperCoauthor Text
  @-}
paperCoauthorAuthor' :: EntityFieldWrapper (Entity User) PaperCoauthor Text
paperCoauthorAuthor' = EntityFieldWrapper PaperCoauthorAuthor
