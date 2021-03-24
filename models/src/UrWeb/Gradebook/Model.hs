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

module UrWeb.Gradebook.Model
  ( migrateAll
  , mkUser
  , mkRootAdmin
  , mkCourse
  , mkInstructor
  , mkTA
  , mkStudent
  , User
  , RootAdmin
  , Course
  , Instructor
  , TA
  , Student
  , userId'
  , userName'
  , userPass'
  , rootAdminId'
  , rootAdminUser'
  , courseId'
  , courseName'
  , instructorId'
  , instructorCourse'
  , instructorUser'
  , tAId'
  , tACourse'
  , tAUser'
  , studentId'
  , studentCourse'
  , studentUser'
  , studentGrade'
  , UserId
  , RootAdminId
  , CourseId
  , InstructorId
  , TAId
  , StudentId
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
  

Course
  name String
  

Instructor
  course CourseId
  user UserId
  

TA
  course CourseId
  user UserId
  

Student
  course CourseId
  user UserId
  grade String Maybe
  
|]

--------------------------------------------------------------------------------
-- | Predicates
--------------------------------------------------------------------------------

{-@ measure isAdmin :: UserId -> Bool @-}

{-@ measure isInstructor :: UserId -> CourseId -> Bool @-}

{-@ measure isTA :: UserId -> CourseId -> Bool @-}

--------------------------------------------------------------------------------
-- | Policies
--------------------------------------------------------------------------------

{-@ predicate IsAdmin USER = isAdmin (entityKey USER) @-}

{-@ predicate IsInstructorT USER ROW = isInstructor (entityKey USER) (tACourse (entityVal ROW)) @-}

{-@ predicate IsInstructorS USER ROW = isInstructor (entityKey USER) (studentCourse (entityVal ROW)) @-}

{-@ predicate IsTAS USER ROW = isTA (entityKey USER) (studentCourse (entityVal ROW)) @-}

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

{-@ invariant {v: Entity RootAdmin | isAdmin (rootAdminUser (entityVal v))} @-}

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

-- * Course
{-@ mkCourse ::
        x_0: String
     -> StormRecord <{\row -> courseName (entityVal row) == x_0},
                     {\_ user -> IsAdmin user},
                     {\x_0 x_1 -> False}>
                     (Entity User) Course
  @-}
mkCourse :: String -> StormRecord (Entity User) Course
mkCourse x_0 = StormRecord (Course x_0)

{-@ invariant {v: Entity Course | v == getJust (entityKey v)} @-}



{-@ assume courseId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Course CourseId
  @-}
courseId' :: EntityFieldWrapper (Entity User) Course CourseId
courseId' = EntityFieldWrapper CourseId

{-@ measure courseName :: Course -> String @-}

{-@ measure courseNameCap :: Entity Course -> Bool @-}

{-@ assume courseName' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == courseName (entityVal row)},
                          {\field row -> field == courseName (entityVal row)},
                          {\old -> courseNameCap old},
                          {\old _ _ -> courseNameCap old}>
                          (Entity User) Course String
  @-}
courseName' :: EntityFieldWrapper (Entity User) Course String
courseName' = EntityFieldWrapper CourseName

-- * Instructor
{-@ mkInstructor ::
        x_0: CourseId
     -> x_1: UserId
     -> StormRecord <{\row -> instructorCourse (entityVal row) == x_0 && instructorUser (entityVal row) == x_1},
                     {\_ user -> IsAdmin user},
                     {\x_0 x_1 -> False}>
                     (Entity User) Instructor
  @-}
mkInstructor :: CourseId -> UserId -> StormRecord (Entity User) Instructor
mkInstructor x_0 x_1 = StormRecord (Instructor x_0 x_1)

{-@ invariant {v: Entity Instructor | v == getJust (entityKey v)} @-}

{-@ invariant {v: Entity Instructor | isInstructor (instructorUser (entityVal v)) (instructorCourse (entityVal v))} @-}

{-@ assume instructorId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Instructor InstructorId
  @-}
instructorId' :: EntityFieldWrapper (Entity User) Instructor InstructorId
instructorId' = EntityFieldWrapper InstructorId

{-@ measure instructorCourse :: Instructor -> CourseId @-}

{-@ measure instructorCourseCap :: Entity Instructor -> Bool @-}

{-@ assume instructorCourse' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == instructorCourse (entityVal row)},
                          {\field row -> field == instructorCourse (entityVal row)},
                          {\old -> instructorCourseCap old},
                          {\x_0 x_1 x_2 -> ((IsAdmin x_2)) => (instructorCourseCap x_0)}>
                          (Entity User) Instructor CourseId
  @-}
instructorCourse' :: EntityFieldWrapper (Entity User) Instructor CourseId
instructorCourse' = EntityFieldWrapper InstructorCourse

{-@ measure instructorUser :: Instructor -> UserId @-}

{-@ measure instructorUserCap :: Entity Instructor -> Bool @-}

{-@ assume instructorUser' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == instructorUser (entityVal row)},
                          {\field row -> field == instructorUser (entityVal row)},
                          {\old -> instructorUserCap old},
                          {\x_0 x_1 x_2 -> ((IsAdmin x_2)) => (instructorUserCap x_0)}>
                          (Entity User) Instructor UserId
  @-}
instructorUser' :: EntityFieldWrapper (Entity User) Instructor UserId
instructorUser' = EntityFieldWrapper InstructorUser

-- * TA
{-@ mkTA ::
        x_0: CourseId
     -> x_1: UserId
     -> StormRecord <{\row -> tACourse (entityVal row) == x_0 && tAUser (entityVal row) == x_1},
                     {\row user -> IsAdmin user || IsInstructorT user row},
                     {\x_0 x_1 -> False}>
                     (Entity User) TA
  @-}
mkTA :: CourseId -> UserId -> StormRecord (Entity User) TA
mkTA x_0 x_1 = StormRecord (TA x_0 x_1)

{-@ invariant {v: Entity TA | v == getJust (entityKey v)} @-}

{-@ invariant {v: Entity TA | isTA (tAUser (entityVal v)) (tACourse (entityVal v))} @-}

{-@ assume tAId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) TA TAId
  @-}
tAId' :: EntityFieldWrapper (Entity User) TA TAId
tAId' = EntityFieldWrapper TAId

{-@ measure tACourse :: TA -> CourseId @-}

{-@ measure tACourseCap :: Entity TA -> Bool @-}

{-@ assume tACourse' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == tACourse (entityVal row)},
                          {\field row -> field == tACourse (entityVal row)},
                          {\old -> tACourseCap old},
                          {\x_0 x_1 x_2 -> ((IsAdmin x_2)) => (tACourseCap x_0)}>
                          (Entity User) TA CourseId
  @-}
tACourse' :: EntityFieldWrapper (Entity User) TA CourseId
tACourse' = EntityFieldWrapper TACourse

{-@ measure tAUser :: TA -> UserId @-}

{-@ measure tAUserCap :: Entity TA -> Bool @-}

{-@ assume tAUser' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == tAUser (entityVal row)},
                          {\field row -> field == tAUser (entityVal row)},
                          {\old -> tAUserCap old},
                          {\x_0 x_1 x_2 -> ((IsAdmin x_2) || (IsInstructorT x_2 x_0)) => (tAUserCap x_0)}>
                          (Entity User) TA UserId
  @-}
tAUser' :: EntityFieldWrapper (Entity User) TA UserId
tAUser' = EntityFieldWrapper TAUser

-- * Student
{-@ mkStudent ::
        x_0: CourseId
     -> x_1: UserId
     -> x_2: Maybe String
     -> StormRecord <{\row -> studentCourse (entityVal row) == x_0 && studentUser (entityVal row) == x_1 && studentGrade (entityVal row) == x_2},
                     {\row user -> IsAdmin user || IsInstructorS user row},
                     {\x_0 x_1 -> (IsInstructorS x_1 x_0 || studentUser (entityVal x_0) == entityKey x_1 || IsTAS x_1 x_0)}>
                     (Entity User) Student
  @-}
mkStudent :: CourseId -> UserId -> Maybe String -> StormRecord (Entity User) Student
mkStudent x_0 x_1 x_2 = StormRecord (Student x_0 x_1 x_2)

{-@ invariant {v: Entity Student | v == getJust (entityKey v)} @-}



{-@ assume studentId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Student StudentId
  @-}
studentId' :: EntityFieldWrapper (Entity User) Student StudentId
studentId' = EntityFieldWrapper StudentId

{-@ measure studentCourse :: Student -> CourseId @-}

{-@ measure studentCourseCap :: Entity Student -> Bool @-}

{-@ assume studentCourse' ::
      EntityFieldWrapper <{\x_0 x_1 -> (IsInstructorS x_1 x_0 || studentUser (entityVal x_0) == entityKey x_1 || IsTAS x_1 x_0)},
                          {\row field -> field == studentCourse (entityVal row)},
                          {\field row -> field == studentCourse (entityVal row)},
                          {\old -> studentCourseCap old},
                          {\x_0 x_1 x_2 -> ((IsAdmin x_2)) => (studentCourseCap x_0)}>
                          (Entity User) Student CourseId
  @-}
studentCourse' :: EntityFieldWrapper (Entity User) Student CourseId
studentCourse' = EntityFieldWrapper StudentCourse

{-@ measure studentUser :: Student -> UserId @-}

{-@ measure studentUserCap :: Entity Student -> Bool @-}

{-@ assume studentUser' ::
      EntityFieldWrapper <{\x_0 x_1 -> (IsInstructorS x_1 x_0 || studentUser (entityVal x_0) == entityKey x_1 || IsTAS x_1 x_0)},
                          {\row field -> field == studentUser (entityVal row)},
                          {\field row -> field == studentUser (entityVal row)},
                          {\old -> studentUserCap old},
                          {\x_0 x_1 x_2 -> ((IsAdmin x_2) || (IsInstructorS x_2 x_0)) => (studentUserCap x_0)}>
                          (Entity User) Student UserId
  @-}
studentUser' :: EntityFieldWrapper (Entity User) Student UserId
studentUser' = EntityFieldWrapper StudentUser

{-@ measure studentGrade :: Student -> (Maybe String) @-}

{-@ measure studentGradeCap :: Entity Student -> Bool @-}

{-@ assume studentGrade' ::
      EntityFieldWrapper <{\x_0 x_1 -> (IsInstructorS x_1 x_0 || studentUser (entityVal x_0) == entityKey x_1 || IsTAS x_1 x_0)},
                          {\row field -> field == studentGrade (entityVal row)},
                          {\field row -> field == studentGrade (entityVal row)},
                          {\old -> studentGradeCap old},
                          {\old _ _ -> studentGradeCap old}>
                          (Entity User) Student (Maybe String)
  @-}
studentGrade' :: EntityFieldWrapper (Entity User) Student (Maybe String)
studentGrade' = EntityFieldWrapper StudentGrade
