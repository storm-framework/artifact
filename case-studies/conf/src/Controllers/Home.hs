{-# LANGUAGE OverloadedStrings #-}

{-@ LIQUID "--no-pattern-inline" @-}

module Controllers.Home where

-- I get a liquid haskell error if I don't import this
import           Data.Int                       ( Int64 )
import           Data.Text                      ( Text )
import           Text.Mustache                  ( (~>)
                                                , ToMustache(..)
                                                )
import qualified Text.Mustache.Types           as Mustache

import           Storm.Core
import           Storm.Actions
import           Storm.Infrastructure
import           Storm.Templates
import           Storm.Frankie
import           Storm.Helpers
import           Storm.Filters
import           Model

import           Controllers
import           Database.Persist.Sql           ( fromSqlKey )
import           Control.Monad                  ( when )

data Home = HomeAuthor [PaperData] | HomeChair [PaperData]

instance TemplateData Home where
  templateFile = "home.html.mustache"

  toMustache (HomeAuthor papers) = Mustache.object
    ["title" ~> ("My Paper" :: String), "prefix" ~> ("" :: String), "papers" ~> papers]
  toMustache (HomeChair papers) = Mustache.object
    ["title" ~> ("Papers" :: String), "prefix" ~> ("/chair" :: String), "papers" ~> papers]

instance ToMustache PaperData where
  toMustache (PaperData paperId paperTitle) =
    Mustache.object ["paperId" ~> paperId, "title" ~> paperTitle]

data PaperData = PaperData
  { paperDataId :: PaperId
  , paperDataTitle :: Text
  }

{-@ homeAuthor :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
homeAuthor :: Controller ()
homeAuthor = do
  viewer    <- requireAuthUser
  viewerId  <- project userId' viewer
  papers    <- selectList (paperAuthor' ==. viewerId)
  paperData <- projectList2 (paperId', paperTitle') papers
  respondHtml $ HomeAuthor (map (uncurry PaperData) paperData)

{-@ homeChair :: TaggedT<{\_ -> False}, {\_ -> True}> _ _ _ @-}
homeChair :: Controller ()
homeChair = do
  viewer <- requireAuthUser
  level  <- project userLevel' viewer
  if level /= "chair"
    then respondTagged forbidden
    else do
      papers    <- selectList trueF
      paperData <- projectList2 (paperId', paperTitle') papers
      respondHtml $ HomeChair (map (uncurry PaperData) paperData)
