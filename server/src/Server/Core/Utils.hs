{-# LANGUAGE FlexibleContexts #-}

module Server.Core.Utils where

-- https://www.reddit.com/r/haskelltil/comments/3mj2ib/convenience_functions_for_reading_environment/

import qualified Data.ByteString.Char8 as B
import Data.Coerce
import Data.Map.Append
import Data.Semigroup (First (..))
import qualified Data.Text as T
import Database.Esqueleto (entityKey)
import Database.Esqueleto.Experimental (Entity)
import RIO
import qualified RIO.Map as Map
import RIO.Partial (read)
import Server.Database.Model
import System.Environment (getEnv, lookupEnv)

fromEnv :: (Read a) => a -> String -> IO a
fromEnv d = fmap (maybe d read) . lookupEnv

readEnv :: Text -> IO ByteString
readEnv = fmap B.pack . getEnv . T.unpack

type GroupedQuestionWithTagsSemigroup =
  AppendMap
    (Key Question)
    (First (Entity Question), AppendMap (Key Tag) (First (Entity Tag)))

makeSingleQuestionWithTags :: (Entity Question, Entity Tag) -> GroupedQuestionWithTagsSemigroup
makeSingleQuestionWithTags (question, tag) =
  AppendMap $
    Map.singleton
      (entityKey question)
      (First question, AppendMap $ Map.singleton (entityKey tag) (First tag))

groupWith :: (Monoid m, Coercible m b) => (r -> m) -> [r] -> b
groupWith fn =
  coerce . mconcat . fmap fn

groupQuestionAndTags :: [(Entity Question, Entity Tag)] -> GroupedQuestionWithTagsSemigroup
groupQuestionAndTags = groupWith makeSingleQuestionWithTags

transformMap ::
  AppendMap (Key Question) (First (Entity Question), AppendMap (Key Tag) (First (Entity Tag))) ->
  [(Entity Question, [Entity Tag])]
transformMap map =
  fmap
    ( \(First parentA, childrenA) ->
        ( parentA,
          fmap getFirst $ Map.elems $ unAppendMap childrenA
        )
    )
    (Map.elems $ unAppendMap map)

groupQueryResults :: [(Entity Question, Entity Tag)] -> [(Entity Question, [Entity Tag])]
groupQueryResults =
  transformMap . groupQuestionAndTags
