{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.Review where

import           Database.Persist.Sql           ( toSqlKey
                                                , fromSqlKey
                                                )
import           Data.Int                       ( Int64 )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Text.Encoding             ( decodeUtf8
                                                , encodeUtf8
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.ByteString                ( ByteString )
import           Text.Mustache                  ( (~>)
                                                , ToMustache(..)
                                                )
import qualified Text.Mustache.Types           as Mustache
import           Text.Printf                    ( printf )
import           Text.Read                      ( readMaybe )
import           Frankie

import           Storm.Core
import           Storm.Actions
import           Storm.Filters
import           Storm.Helpers
import           Storm.Infrastructure
import           Storm.Templates
import           Storm.Frankie
import           Storm.Updates
import           Storm.Insert
import           Model
import           Stage

import           Helpers
import           Controllers

import           Control.Monad                  ( when )


------------------------------------------------------------------------------------------------
-- | Edit Review
------------------------------------------------------------------------------------------------

data EditReview = EditReview ReviewId ReviewData | NewReview PaperId

instance TemplateData EditReview where
  templateFile = "review.edit.html.mustache"

  toMustache (NewReview paperId) = Mustache.object ["action" ~> newReviewRoute paperId]
  toMustache (EditReview reviewId review) =
    Mustache.object ["action" ~> reviewEditRoute reviewId, "review" ~> review]

{-@ reviewNew :: _ -> TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
reviewNew :: PaperId -> Controller ()
reviewNew paperId = do
  viewer   <- requireAuthUser
  viewerId <- project userId' viewer
  _        <- checkPcOr forbidden viewer
  _        <- checkStageOr forbidden "review"
  req      <- request
  if reqMethod req == methodPost
    then do
      _ <- selectFirstOr
        forbidden
        (reviewAssignmentPaper' ==. paperId &&: reviewAssignmentUser' ==. viewerId)
      params <- parseForm
      let score = lookup "score" params >>= readMaybe . Text.unpack
      case (score, lookup "content" params) of
        (Just score, Just content) -> do
          reviewId <- insert (mkReview paperId viewerId content score)
          respondTagged (redirectTo (reviewRoute reviewId))
        _ -> respondTagged badRequest
    else respondTagged forbidden


------------------------------------------------------------------------------------------------
-- | Show Review
------------------------------------------------------------------------------------------------

newtype ShowReview = ShowReview ReviewData

instance TemplateData ShowReview where
  templateFile = "review.show.html.mustache"

  toMustache (ShowReview review) = Mustache.object ["review" ~> review]

{-@ updateReview :: ReviewId -> TaggedT<{\v -> v == currentUser 0}, {\_ -> True}> _ _ _ @-}
updateReview :: ReviewId -> Controller ()
updateReview reviewId = do
  viewer   <- requireAuthUser
  viewerId <- project userId' viewer
  _        <- checkPcOr forbidden viewer
  _        <- checkStageOr forbidden "review"
  review   <- selectFirstOr notFound (reviewId' ==. reviewId)
  paperId  <- project reviewPaper' review
  _ <- selectFirstOr forbidden
                     (reviewAssignmentPaper' ==. paperId &&: reviewAssignmentUser' ==. viewerId)

  params <- parseForm
  case (lookup "content" params, lookup "score" params) of
    (Just content, Just score) -> do
      let up1 = reviewContent' `assign` content
      let up2 = reviewScore' `assign` read (Text.unpack score)
      _ <- updateWhere (reviewId' ==. reviewId) (up1 `combine` up2)
      return ()
    _ -> return ()

{-@ reviewShow :: _ -> TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
reviewShow :: ReviewId -> Controller ()
reviewShow reviewId = do
  viewer   <- requireAuthUser
  viewerId <- project userId' viewer

  req      <- request
  if reqMethod req == methodPost
    then do
      _ <- updateReview reviewId
      return ()
    else return ()

  review <- selectFirstOr notFound (reviewId' ==. reviewId)
  isPC   <- pc viewer
  case (isPC, currentStage == "public") of
    (True, _) -> do
      reviewData <- project2 (reviewScore', reviewContent') review
      respondHtml $ ShowReview (uncurry ReviewData reviewData)
    (_, True) -> do
      paperId <- project reviewPaper' review
      paper   <- selectFirst (paperId' ==. paperId &&: paperAuthor' ==. viewerId)
      case paper of
        Just _ -> do
          reviewData <- project2 (reviewScore', reviewContent') review
          respondHtml $ ShowReview (uncurry ReviewData reviewData)
        Nothing -> return ()
    _ -> return ()

  respondTagged forbidden


------------------------------------------------------------------------------------------------
-- | Helpers
------------------------------------------------------------------------------------------------

data ReviewData = ReviewData { reviewDataScore :: Int, reviewDataContent :: Text}

instance ToMustache ReviewData where
  toMustache (ReviewData score content) = Mustache.object ["score" ~> score, "content" ~> content]

reviewRoute :: ReviewId -> ByteString
reviewRoute reviewId = encodeUtf8 (Text.pack path)
  where path = printf "/review/%d/edit" (fromSqlKey reviewId)

reviewEditRoute :: ReviewId -> String
reviewEditRoute reviewId = printf "/review/%d/edit" (fromSqlKey reviewId)

newReviewRoute :: PaperId -> String
newReviewRoute paperId = printf "/paper/%d/review" (fromSqlKey paperId)
