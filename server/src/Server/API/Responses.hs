{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Server.API.Responses where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Esqueleto.Experimental
import RIO
import Server.Database.Model (Question (..))

mkQuestionDTO :: Entity Question -> [Text] -> QuestionDTO
mkQuestionDTO (Entity questionId question) tags =
  QuestionDTO
    { id = fromSqlKey questionId,
      title = t,
      content = c,
      userId = fromSqlKey uId,
      createdAt = cr,
      updatedAt = up,
      tags = tags
    }
  where
    t = questionTitle question
    c = questionContent question
    uId = questionUserId question
    cr = questionCreatedAt question
    up = questionUpdatedAt question

data QuestionDTO = QuestionDTO
  { id :: Int64,
    title :: Text,
    content :: Text,
    userId :: Int64,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    tags :: [Text]
  }
  deriving (Eq, Show, ToJSON, FromJSON, Generic)
