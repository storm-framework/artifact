{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.AssignmentShow where

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

data AssignmentData = AssignmentData { assignmentDataDescription :: Text }

instance TemplateData AssignmentData where
  templateFile = "assignment.show.html.mustache"
  toMustache (AssignmentData description) =
    Mustache.object ["description" ~> description]


{-@ assignmentShow :: _ -> TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
assignmentShow :: AssignmentId -> Controller ()
assignmentShow assignmentId = do
  viewer          <- requireAuthUser
  viewerId        <- project userId' viewer
  maybeAssignment <- selectFirst (assignmentId' ==. assignmentId)

  assignment      <- case maybeAssignment of
    Just assignment -> returnTagged assignment
    Nothing         -> respondTagged notFound

  courseId   <- project assignmentCourse' assignment

  enrollment <- selectFirst
    (enrollmentStudent' ==. viewerId &&: enrollmentCourse' ==. courseId)
  instruction <- selectFirst
    (courseInstructorInstructor' ==. viewerId &&: courseInstructorCourse' ==. courseId)
  description <- case (enrollment, instruction) of
    (Just _, _) -> project assignmentDescription' assignment
    (_, Just _) -> project assignmentDescription' assignment
    _           -> respondTagged forbidden

  respondHtml (AssignmentData description)
