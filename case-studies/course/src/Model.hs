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
  , mkCourse
  , mkCourseInstructor
  , mkEnrollment
  , mkAssignment
  , mkSubmission
  , User
  , Course
  , CourseInstructor
  , Enrollment
  , Assignment
  , Submission
  , userId'
  , userUsername'
  , userEmail'
  , userName'
  , userRole'
  , courseId'
  , courseName'
  , courseInstructorId'
  , courseInstructorCourse'
  , courseInstructorInstructor'
  , enrollmentId'
  , enrollmentStudent'
  , enrollmentCourse'
  , enrollmentGrade'
  , assignmentId'
  , assignmentName'
  , assignmentCourse'
  , assignmentDescription'
  , submissionId'
  , submissionAssignment'
  , submissionAuthor'
  , submissionContent'
  , submissionGrade'
  , UserId
  , CourseId
  , CourseInstructorId
  , EnrollmentId
  , AssignmentId
  , SubmissionId
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

import Data.Text ( Text )

--------------------------------------------------------------------------------
-- | Inline
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- | Persistent
--------------------------------------------------------------------------------

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  username Text
  email Text
  name Text
  role String
  

Course
  name Text
  

CourseInstructor
  course CourseId
  instructor UserId
  

Enrollment
  student UserId
  course CourseId
  grade String
  

Assignment
  name Text
  course CourseId
  description Text
  

Submission
  assignment AssignmentId
  author UserId
  content Text
  grade String
  
|]

--------------------------------------------------------------------------------
-- | Predicates
--------------------------------------------------------------------------------

{-@ measure isInstructor :: UserId -> CourseId -> Bool @-}

{-@ measure isEnrolled :: UserId -> CourseId -> Bool @-}

--------------------------------------------------------------------------------
-- | Policies
--------------------------------------------------------------------------------

{-@ predicate IsSelf USER VIEWER = entityKey USER == entityKey VIEWER @-}

{-@ predicate StudentOrInstructor ENROLLMENT VIEWER = entityKey VIEWER == enrollmentStudent (entityVal ENROLLMENT) || isInstructor (entityKey VIEWER) (enrollmentCourse (entityVal ENROLLMENT)) @-}

{-@ predicate EnrolledOrInstructor ASSIGNMENT VIEWER = isEnrolled (entityKey VIEWER) (assignmentCourse (entityVal ASSIGNMENT)) || isInstructor (entityKey VIEWER) (assignmentCourse (entityVal ASSIGNMENT)) @-}

{-@ predicate AuthorOrInstructor SUBMISSION VIEWER = submissionAuthor (entityVal SUBMISSION) == entityKey VIEWER || isInstructor (entityKey VIEWER) (assignmentCourse (entityVal (getJust (submissionAssignment (entityVal SUBMISSION))))) @-}

--------------------------------------------------------------------------------
-- | Records
--------------------------------------------------------------------------------

{-@ measure getJust :: Key record -> Entity record @-}

-- * User
{-@ mkUser ::
        x_0: Text
     -> x_1: Text
     -> x_2: Text
     -> x_3: String
     -> StormRecord <{\row -> userUsername (entityVal row) == x_0 && userEmail (entityVal row) == x_1 && userName (entityVal row) == x_2 && userRole (entityVal row) == x_3},
                     {\_ _ -> True},
                     {\x_0 x_1 -> (entityKey x_0 == entityKey x_1)}>
                     (Entity User) User
  @-}
mkUser :: Text -> Text -> Text -> String -> StormRecord (Entity User) User
mkUser x_0 x_1 x_2 x_3 = StormRecord (User x_0 x_1 x_2 x_3)

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

{-@ measure userEmail :: User -> Text @-}

{-@ measure userEmailCap :: Entity User -> Bool @-}

{-@ assume userEmail' ::
      EntityFieldWrapper <{\x_0 x_1 -> (entityKey x_0 == entityKey x_1)},
                          {\row field -> field == userEmail (entityVal row)},
                          {\field row -> field == userEmail (entityVal row)},
                          {\old -> userEmailCap old},
                          {\old _ _ -> userEmailCap old}>
                          (Entity User) User Text
  @-}
userEmail' :: EntityFieldWrapper (Entity User) User Text
userEmail' = EntityFieldWrapper UserEmail

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

{-@ measure userRole :: User -> String @-}

{-@ measure userRoleCap :: Entity User -> Bool @-}

{-@ assume userRole' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userRole (entityVal row)},
                          {\field row -> field == userRole (entityVal row)},
                          {\old -> userRoleCap old},
                          {\old _ _ -> userRoleCap old}>
                          (Entity User) User String
  @-}
userRole' :: EntityFieldWrapper (Entity User) User String
userRole' = EntityFieldWrapper UserRole

-- * Course
{-@ mkCourse ::
        x_0: Text
     -> StormRecord <{\row -> courseName (entityVal row) == x_0},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) Course
  @-}
mkCourse :: Text -> StormRecord (Entity User) Course
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

{-@ measure courseName :: Course -> Text @-}

{-@ measure courseNameCap :: Entity Course -> Bool @-}

{-@ assume courseName' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == courseName (entityVal row)},
                          {\field row -> field == courseName (entityVal row)},
                          {\old -> courseNameCap old},
                          {\old _ _ -> courseNameCap old}>
                          (Entity User) Course Text
  @-}
courseName' :: EntityFieldWrapper (Entity User) Course Text
courseName' = EntityFieldWrapper CourseName

-- * CourseInstructor
{-@ mkCourseInstructor ::
        x_0: CourseId
     -> x_1: UserId
     -> StormRecord <{\row -> courseInstructorCourse (entityVal row) == x_0 && courseInstructorInstructor (entityVal row) == x_1},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) CourseInstructor
  @-}
mkCourseInstructor :: CourseId -> UserId -> StormRecord (Entity User) CourseInstructor
mkCourseInstructor x_0 x_1 = StormRecord (CourseInstructor x_0 x_1)

{-@ invariant {v: Entity CourseInstructor | v == getJust (entityKey v)} @-}

{-@ invariant {v: Entity CourseInstructor | isInstructor (courseInstructorInstructor (entityVal v)) (courseInstructorCourse (entityVal v))} @-}

{-@ assume courseInstructorId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) CourseInstructor CourseInstructorId
  @-}
courseInstructorId' :: EntityFieldWrapper (Entity User) CourseInstructor CourseInstructorId
courseInstructorId' = EntityFieldWrapper CourseInstructorId

{-@ measure courseInstructorCourse :: CourseInstructor -> CourseId @-}

{-@ measure courseInstructorCourseCap :: Entity CourseInstructor -> Bool @-}

{-@ assume courseInstructorCourse' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == courseInstructorCourse (entityVal row)},
                          {\field row -> field == courseInstructorCourse (entityVal row)},
                          {\old -> courseInstructorCourseCap old},
                          {\old _ _ -> courseInstructorCourseCap old}>
                          (Entity User) CourseInstructor CourseId
  @-}
courseInstructorCourse' :: EntityFieldWrapper (Entity User) CourseInstructor CourseId
courseInstructorCourse' = EntityFieldWrapper CourseInstructorCourse

{-@ measure courseInstructorInstructor :: CourseInstructor -> UserId @-}

{-@ measure courseInstructorInstructorCap :: Entity CourseInstructor -> Bool @-}

{-@ assume courseInstructorInstructor' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == courseInstructorInstructor (entityVal row)},
                          {\field row -> field == courseInstructorInstructor (entityVal row)},
                          {\old -> courseInstructorInstructorCap old},
                          {\old _ _ -> courseInstructorInstructorCap old}>
                          (Entity User) CourseInstructor UserId
  @-}
courseInstructorInstructor' :: EntityFieldWrapper (Entity User) CourseInstructor UserId
courseInstructorInstructor' = EntityFieldWrapper CourseInstructorInstructor

-- * Enrollment
{-@ mkEnrollment ::
        x_0: UserId
     -> x_1: CourseId
     -> x_2: String
     -> StormRecord <{\row -> enrollmentStudent (entityVal row) == x_0 && enrollmentCourse (entityVal row) == x_1 && enrollmentGrade (entityVal row) == x_2},
                     {\_ _ -> True},
                     {\x_0 x_1 -> (entityKey x_1 == enrollmentStudent (entityVal x_0) || isInstructor (entityKey x_1) (enrollmentCourse (entityVal x_0)))}>
                     (Entity User) Enrollment
  @-}
mkEnrollment :: UserId -> CourseId -> String -> StormRecord (Entity User) Enrollment
mkEnrollment x_0 x_1 x_2 = StormRecord (Enrollment x_0 x_1 x_2)

{-@ invariant {v: Entity Enrollment | v == getJust (entityKey v)} @-}

{-@ invariant {v: Entity Enrollment | isEnrolled (enrollmentStudent (entityVal v)) (enrollmentCourse (entityVal v))} @-}

{-@ assume enrollmentId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Enrollment EnrollmentId
  @-}
enrollmentId' :: EntityFieldWrapper (Entity User) Enrollment EnrollmentId
enrollmentId' = EntityFieldWrapper EnrollmentId

{-@ measure enrollmentStudent :: Enrollment -> UserId @-}

{-@ measure enrollmentStudentCap :: Entity Enrollment -> Bool @-}

{-@ assume enrollmentStudent' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == enrollmentStudent (entityVal row)},
                          {\field row -> field == enrollmentStudent (entityVal row)},
                          {\old -> enrollmentStudentCap old},
                          {\old _ _ -> enrollmentStudentCap old}>
                          (Entity User) Enrollment UserId
  @-}
enrollmentStudent' :: EntityFieldWrapper (Entity User) Enrollment UserId
enrollmentStudent' = EntityFieldWrapper EnrollmentStudent

{-@ measure enrollmentCourse :: Enrollment -> CourseId @-}

{-@ measure enrollmentCourseCap :: Entity Enrollment -> Bool @-}

{-@ assume enrollmentCourse' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == enrollmentCourse (entityVal row)},
                          {\field row -> field == enrollmentCourse (entityVal row)},
                          {\old -> enrollmentCourseCap old},
                          {\old _ _ -> enrollmentCourseCap old}>
                          (Entity User) Enrollment CourseId
  @-}
enrollmentCourse' :: EntityFieldWrapper (Entity User) Enrollment CourseId
enrollmentCourse' = EntityFieldWrapper EnrollmentCourse

{-@ measure enrollmentGrade :: Enrollment -> String @-}

{-@ measure enrollmentGradeCap :: Entity Enrollment -> Bool @-}

{-@ assume enrollmentGrade' ::
      EntityFieldWrapper <{\x_0 x_1 -> (entityKey x_1 == enrollmentStudent (entityVal x_0) || isInstructor (entityKey x_1) (enrollmentCourse (entityVal x_0)))},
                          {\row field -> field == enrollmentGrade (entityVal row)},
                          {\field row -> field == enrollmentGrade (entityVal row)},
                          {\old -> enrollmentGradeCap old},
                          {\old _ _ -> enrollmentGradeCap old}>
                          (Entity User) Enrollment String
  @-}
enrollmentGrade' :: EntityFieldWrapper (Entity User) Enrollment String
enrollmentGrade' = EntityFieldWrapper EnrollmentGrade

-- * Assignment
{-@ mkAssignment ::
        x_0: Text
     -> x_1: CourseId
     -> x_2: Text
     -> StormRecord <{\row -> assignmentName (entityVal row) == x_0 && assignmentCourse (entityVal row) == x_1 && assignmentDescription (entityVal row) == x_2},
                     {\_ _ -> True},
                     {\x_0 x_1 -> (isEnrolled (entityKey x_1) (assignmentCourse (entityVal x_0)) || isInstructor (entityKey x_1) (assignmentCourse (entityVal x_0)))}>
                     (Entity User) Assignment
  @-}
mkAssignment :: Text -> CourseId -> Text -> StormRecord (Entity User) Assignment
mkAssignment x_0 x_1 x_2 = StormRecord (Assignment x_0 x_1 x_2)

{-@ invariant {v: Entity Assignment | v == getJust (entityKey v)} @-}



{-@ assume assignmentId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Assignment AssignmentId
  @-}
assignmentId' :: EntityFieldWrapper (Entity User) Assignment AssignmentId
assignmentId' = EntityFieldWrapper AssignmentId

{-@ measure assignmentName :: Assignment -> Text @-}

{-@ measure assignmentNameCap :: Entity Assignment -> Bool @-}

{-@ assume assignmentName' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == assignmentName (entityVal row)},
                          {\field row -> field == assignmentName (entityVal row)},
                          {\old -> assignmentNameCap old},
                          {\old _ _ -> assignmentNameCap old}>
                          (Entity User) Assignment Text
  @-}
assignmentName' :: EntityFieldWrapper (Entity User) Assignment Text
assignmentName' = EntityFieldWrapper AssignmentName

{-@ measure assignmentCourse :: Assignment -> CourseId @-}

{-@ measure assignmentCourseCap :: Entity Assignment -> Bool @-}

{-@ assume assignmentCourse' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == assignmentCourse (entityVal row)},
                          {\field row -> field == assignmentCourse (entityVal row)},
                          {\old -> assignmentCourseCap old},
                          {\old _ _ -> assignmentCourseCap old}>
                          (Entity User) Assignment CourseId
  @-}
assignmentCourse' :: EntityFieldWrapper (Entity User) Assignment CourseId
assignmentCourse' = EntityFieldWrapper AssignmentCourse

{-@ measure assignmentDescription :: Assignment -> Text @-}

{-@ measure assignmentDescriptionCap :: Entity Assignment -> Bool @-}

{-@ assume assignmentDescription' ::
      EntityFieldWrapper <{\x_0 x_1 -> (isEnrolled (entityKey x_1) (assignmentCourse (entityVal x_0)) || isInstructor (entityKey x_1) (assignmentCourse (entityVal x_0)))},
                          {\row field -> field == assignmentDescription (entityVal row)},
                          {\field row -> field == assignmentDescription (entityVal row)},
                          {\old -> assignmentDescriptionCap old},
                          {\old _ _ -> assignmentDescriptionCap old}>
                          (Entity User) Assignment Text
  @-}
assignmentDescription' :: EntityFieldWrapper (Entity User) Assignment Text
assignmentDescription' = EntityFieldWrapper AssignmentDescription

-- * Submission
{-@ mkSubmission ::
        x_0: AssignmentId
     -> x_1: UserId
     -> x_2: Text
     -> x_3: String
     -> StormRecord <{\row -> submissionAssignment (entityVal row) == x_0 && submissionAuthor (entityVal row) == x_1 && submissionContent (entityVal row) == x_2 && submissionGrade (entityVal row) == x_3},
                     {\_ _ -> True},
                     {\x_0 x_1 -> (submissionAuthor (entityVal x_0) == entityKey x_1 || isInstructor (entityKey x_1) (assignmentCourse (entityVal (getJust (submissionAssignment (entityVal x_0))))))}>
                     (Entity User) Submission
  @-}
mkSubmission :: AssignmentId -> UserId -> Text -> String -> StormRecord (Entity User) Submission
mkSubmission x_0 x_1 x_2 x_3 = StormRecord (Submission x_0 x_1 x_2 x_3)

{-@ invariant {v: Entity Submission | v == getJust (entityKey v)} @-}



{-@ assume submissionId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Submission SubmissionId
  @-}
submissionId' :: EntityFieldWrapper (Entity User) Submission SubmissionId
submissionId' = EntityFieldWrapper SubmissionId

{-@ measure submissionAssignment :: Submission -> AssignmentId @-}

{-@ measure submissionAssignmentCap :: Entity Submission -> Bool @-}

{-@ assume submissionAssignment' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == submissionAssignment (entityVal row)},
                          {\field row -> field == submissionAssignment (entityVal row)},
                          {\old -> submissionAssignmentCap old},
                          {\old _ _ -> submissionAssignmentCap old}>
                          (Entity User) Submission AssignmentId
  @-}
submissionAssignment' :: EntityFieldWrapper (Entity User) Submission AssignmentId
submissionAssignment' = EntityFieldWrapper SubmissionAssignment

{-@ measure submissionAuthor :: Submission -> UserId @-}

{-@ measure submissionAuthorCap :: Entity Submission -> Bool @-}

{-@ assume submissionAuthor' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == submissionAuthor (entityVal row)},
                          {\field row -> field == submissionAuthor (entityVal row)},
                          {\old -> submissionAuthorCap old},
                          {\old _ _ -> submissionAuthorCap old}>
                          (Entity User) Submission UserId
  @-}
submissionAuthor' :: EntityFieldWrapper (Entity User) Submission UserId
submissionAuthor' = EntityFieldWrapper SubmissionAuthor

{-@ measure submissionContent :: Submission -> Text @-}

{-@ measure submissionContentCap :: Entity Submission -> Bool @-}

{-@ assume submissionContent' ::
      EntityFieldWrapper <{\x_0 x_1 -> (submissionAuthor (entityVal x_0) == entityKey x_1 || isInstructor (entityKey x_1) (assignmentCourse (entityVal (getJust (submissionAssignment (entityVal x_0))))))},
                          {\row field -> field == submissionContent (entityVal row)},
                          {\field row -> field == submissionContent (entityVal row)},
                          {\old -> submissionContentCap old},
                          {\old _ _ -> submissionContentCap old}>
                          (Entity User) Submission Text
  @-}
submissionContent' :: EntityFieldWrapper (Entity User) Submission Text
submissionContent' = EntityFieldWrapper SubmissionContent

{-@ measure submissionGrade :: Submission -> String @-}

{-@ measure submissionGradeCap :: Entity Submission -> Bool @-}

{-@ assume submissionGrade' ::
      EntityFieldWrapper <{\x_0 x_1 -> (submissionAuthor (entityVal x_0) == entityKey x_1 || isInstructor (entityKey x_1) (assignmentCourse (entityVal (getJust (submissionAssignment (entityVal x_0))))))},
                          {\row field -> field == submissionGrade (entityVal row)},
                          {\field row -> field == submissionGrade (entityVal row)},
                          {\old -> submissionGradeCap old},
                          {\old _ _ -> submissionGradeCap old}>
                          (Entity User) Submission String
  @-}
submissionGrade' :: EntityFieldWrapper (Entity User) Submission String
submissionGrade' = EntityFieldWrapper SubmissionGrade
