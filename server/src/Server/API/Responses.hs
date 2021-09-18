{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Server.API.Responses where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Esqueleto.Experimental
import RIO
import Server.Database.Model (Question (..), Tag (..))

mkQuestionDTO :: Entity Question -> [Entity Tag] -> QuestionDTO
mkQuestionDTO (Entity questionId question) tags =
  QuestionDTO
    { id = fromSqlKey questionId,
      title = t,
      content = c,
      userId = fromSqlKey uId,
      createdAt = cr,
      updatedAt = up,
      tags = fmap mkTagDTO tags
    }
  where
    t = questionTitle question
    c = questionContent question
    uId = questionUserId question
    cr = questionCreatedAt question
    up = questionUpdatedAt question

mkTagDTO :: Entity Tag -> TagDTO
mkTagDTO (Entity tagId tag) =
  TagDTO
    { id = fromSqlKey tagId,
      title = tagTitle tag
    }

data QuestionDTO = QuestionDTO
  { id :: Int64,
    title :: Text,
    content :: Text,
    userId :: Int64,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    tags :: [TagDTO]
  }
  deriving (Eq, Show, Ord, ToJSON, FromJSON, Generic)

data TagDTO = TagDTO
  {id :: Int64, title :: Text}
  deriving (Eq, Show, Ord, ToJSON, FromJSON, Generic)
