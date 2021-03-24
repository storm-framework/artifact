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

keyToText :: ToBackendKey SqlBackend record => Key record -> Text
keyToText key = pack . show $ fromSqlKey key
