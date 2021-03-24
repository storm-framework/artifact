{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.CourseIndex where

import           Database.Persist.Sql           ( toSqlKey )
import           Data.Text                      ( Text )
import           Text.Mustache                  ( (~>)
                                                , ToMustache(..)
                                                )
import qualified Text.Mustache.Types           as Mustache
import           Frankie

import           Storm.Core
import           Storm.Actions
import           Storm.Filters
import           Storm.Helpers
import           Storm.Infrastructure
import           Storm.Templates
import           Storm.Frankie
import           Model

import           Controllers
import           Helpers

data CourseIndex = CourseIndex [CourseData]

instance TemplateData CourseIndex where
  templateFile = "course.index.html.mustache"
  toMustache (CourseIndex courses) = Mustache.object ["courses" ~> courses]

data CourseData = CourseData
  { courseDataId :: CourseId
  , courseDataName :: Text
  , courseDataGrade :: String
  }

instance ToMustache CourseData where
  toMustache (CourseData id name grade) = Mustache.object
    [ "course_id" ~> keyToText id
    , "name" ~> name
    , "grade" ~> grade
    ]

{-@ joinWithCourses :: _ -> TaggedT<{\_ -> True}, {\_ ->False}> _ _ _ @-}
joinWithCourses :: [(CourseId, String)] -> Controller [CourseData]
joinWithCourses gradesByCourse = do
  courses     <- selectList (courseId' <-. map fst gradesByCourse)
  coursesById <- projectList2 (courseId', courseName') courses
  returnTagged $ innerJoin CourseData coursesById gradesByCourse


{-@ courseIndex :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
courseIndex :: Controller ()
courseIndex = do
  viewer         <- requireAuthUser
  viewerId       <- project userId' viewer
  enrollments    <- selectList (enrollmentStudent' ==. viewerId)
  gradesByCourse <- projectList2 (enrollmentCourse', enrollmentGrade') enrollments
  courses        <- joinWithCourses gradesByCourse
  respondHtml (CourseIndex courses)
