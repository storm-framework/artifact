{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.SubmissionShow where

import           Database.Persist.Sql           ( toSqlKey )
import           Data.Int                       ( Int64 )
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( decodeUtf8 )
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
import           Storm.Updates
import           Storm.Frankie
import           Model

import           Controllers
import           Helpers

data SubmissionData = SubmissionData
  { submissionDataContent :: Text
  , submissionDataGrade :: String
  }

instance TemplateData SubmissionData where
  templateFile = "submission.show.html.mustache"
  toMustache (SubmissionData content grade) =
    Mustache.object ["content" ~> content, "grade" ~> grade]

{-@ submissionShow :: _ -> TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
submissionShow :: Int64 -> Controller ()
submissionShow sid = do
  let submissionId = toSqlKey sid
  viewer   <- requireAuthUser
  viewerId <- project userId' viewer

  req      <- request
  if reqMethod req == methodPost
    then do
      params <- parseForm
      case lookup "content" params of
        Just content -> updateWhere (submissionId' ==. submissionId)
                                    (submissionContent' `assign` content)
        Nothing      -> returnTagged ()
    else returnTagged ()

  submission   <- selectFirstOr404 (submissionId' ==. submissionId)
  assignmentId <- project submissionAssignment' submission
  authorId     <- project submissionAuthor' submission
  assignment   <- selectFirstOr404 (assignmentId' ==. assignmentId)
  courseId     <- project assignmentCourse' assignment

  instruction  <- selectFirst
    (courseInstructorInstructor' ==. viewerId &&: courseInstructorCourse' ==. courseId)

  (content, grade) <- case (authorId == viewerId, instruction) of
    (True, _     ) -> project2 (submissionContent', submissionGrade') submission
    (_   , Just _) -> project2 (submissionContent', submissionGrade') submission
    _              -> respondTagged forbidden

  respondHtml (SubmissionData content grade)
