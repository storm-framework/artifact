{-# LANGUAGE FlexibleContexts #-}
{-@ LIQUID "--no-pattern-inline" @-}

module Helpers where

import           Data.Text                      ( Text
                                                , pack
                                                )
import           Database.Persist.Sql           ( fromSqlKey
                                                , ToBackendKey
                                                , SqlBackend
                                                )
import           Storm.Core
import           Storm.Actions
import           Storm.Filters
import           Storm.Infrastructure
import           Storm.Templates
import           Storm.Frankie
import           Model
import           Controllers
import           Stage

{-@ pc :: u:_ -> TaggedT<{\_ -> True}, {\_ -> False}> _ _ {v: Bool | v <=> IsPc u} @-}
pc :: Monad m => Entity User -> TaggedT (Entity User) m Bool
pc user = do
  level <- project userLevel' user
  return (level == "chair" || level == "pc")

{-@ chair :: u:_ -> TaggedT<{\_ -> True}, {\_ -> False}> _ _ {v: Bool | v <=> IsChair u} @-}
chair :: Monad m => Entity User -> TaggedT (Entity User) m Bool
chair user = do
  level <- project userLevel' user
  return (level == "chair")

{-@ checkPcOr :: Response -> u:Entity User ->
      TaggedT<{\_ -> True}, {\v -> v == currentUser 0}> _ _ {v: () | IsPc u} @-}
checkPcOr :: Response -> Entity User -> Controller ()
checkPcOr response user = do
  level <- project userLevel' user
  if (level == "chair" || level == "pc") then return () else respondTagged response

{-@ checkChairOr :: Response
                 -> u: Entity User
                 -> TaggedT<{\_ -> True}, {\v -> v == currentUser 0}> _ _ {v: () | IsChair u} @-}
checkChairOr :: Response -> Entity User -> Controller ()
checkChairOr response user = do
  level <- project userLevel' user
  if level == "chair" then return () else respondTagged response

{-@ checkStageOr :: Response -> s:String
      -> TaggedT<{\_ -> True}, {\v -> v == currentUser 0}> _ _ {v: () | s == currentStage} @-}
checkStageOr :: Response -> String -> Controller ()
checkStageOr response stage = if currentStage == stage then return () else respondTagged response

outerJoinBy :: Eq key => (a -> key) -> (b -> key) -> (a -> Maybe b -> c) -> [a] -> [b] -> [c]
outerJoinBy xsKey ysKey f xs ys =
  let ysByKey = map (\y -> (ysKey y, y)) ys in map (\x -> f x (lookup (xsKey x) ysByKey)) xs

outerJoin :: Eq a => (a -> b -> Maybe c -> d) -> [(a, b)] -> [(a, c)] -> [d]
outerJoin f = outerJoinBy fst fst (\x y -> f (fst x) (snd x) (fmap snd y))

innerJoinBy :: Eq key => (a -> key) -> (b -> key) -> (a -> b -> c) -> [a] -> [b] -> [c]
innerJoinBy xsKey ysKey f xs ys =
  let joined = outerJoinBy xsKey ysKey (,) xs ys in [ f x y | (x, Just y) <- joined ]

innerJoin :: Eq a => (a -> b -> c -> d) -> [(a, b)] -> [(a, c)] -> [d]
innerJoin f = innerJoinBy fst fst (\x y -> f (fst x) (snd x) (snd y))
