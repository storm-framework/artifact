{-# LANGUAGE OverloadedStrings #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.PaperIndex where

import           Database.Persist.Sql           ( toSqlKey )
import           Data.Int                       ( Int64 )
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
import           Stage

import           Helpers
import           Controllers


newtype PaperIndex = PaperIndex [RowData]

instance TemplateData PaperIndex where
  templateFile = "paper.index.html.mustache"
  toMustache (PaperIndex papers) = Mustache.object ["papers" ~> papers]

data RowData = RowData
  { paperDataAuthorId :: Maybe UserId
  , paperDataAuthor :: Maybe Text
  , paperDataId :: PaperId
  , paperDataTitle :: Text
  }

instance ToMustache RowData where
  toMustache (RowData authorId authorName paperId paperTitle) = Mustache.object
    [ "paper_id" ~> paperId
    , "author_id" ~> authorId
    , "title" ~> paperTitle
    , "author_name" ~> authorName
    ]


{-@ joinWithAuthors :: _ -> TaggedT<{\_ -> True}, {\_ -> False}> _ _ _ @-}
joinWithAuthors :: [(UserId, (PaperId, Text))] -> Controller [RowData]
joinWithAuthors papersByAuthor = do
  authors     <- selectList (userId' <-. map fst papersByAuthor)
  authorsById <- projectList2 (userId', userName') authors

  return $ innerJoin
    (\authorId (paperId, title) authorName ->
      RowData (Just authorId) (Just authorName) paperId title
    )
    papersByAuthor
    authorsById

{-@ getAllPapers :: TaggedT<{\u -> IsPc u}, {\_ -> False}> _ _ _ @-}
getAllPapers :: Controller [RowData]
getAllPapers = do
  papers    <- selectList trueF
  authorIds <- projectList paperAuthor' papers
  paperData <- projectList2 (paperId', paperTitle') papers
  joinWithAuthors $ zip authorIds paperData


{-@ getAcceptedPapers :: TaggedT<{\_ -> currentStage == "public"}, {\_ -> False}> _ _ _ @-}
getAcceptedPapers :: Controller [RowData]
getAcceptedPapers = do
  papers    <- selectList (paperAccepted' ==. True)
  authorIds <- projectList paperAuthor' papers
  paperData <- projectList2 (paperId', paperTitle') papers
  joinWithAuthors $ zip authorIds paperData


{-@ getMyPapers :: v: _ -> TaggedT<{\u -> (entityKey v) == (entityKey u)}, {\_ -> False}> _ _ _ @-}
getMyPapers :: Entity User -> Controller [RowData]
getMyPapers viewer = do
  viewerId   <- project userId' viewer
  viewerName <- project userName' viewer

  papers     <- selectList (paperAuthor' ==. viewerId)
  paperData  <- projectList2 (paperId', paperTitle') papers

  return $ map (uncurry $ RowData (Just viewerId) (Just viewerName)) paperData


{-@ paperIndex :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
paperIndex :: Controller ()
paperIndex = do
  viewer   <- requireAuthUser
  viewerId <- project userId' viewer
  papers   <- selectList trueF
  paperIds <- projectList paperId' papers
  isPC     <- pc viewer
  papers   <- case (currentStage == "public", isPC) of
    (True, _   ) -> getAcceptedPapers
    (_   , True) -> getAllPapers
    _            -> getMyPapers viewer
  respondHtml (PaperIndex papers)
