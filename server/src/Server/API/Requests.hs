{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Server.API.Requests where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data CreateQuestionRequest = CreateQuestionRequest
  { title :: Text,
    content :: Text,
    userId :: Int
  }
  deriving (Eq, Show, ToJSON, FromJSON, Generic)

data CreateAnswerRequest = CreateAnswerRequest
  { answerContent :: Text,
    answererId :: Int
  } deriving (Eq, Show, ToJSON, FromJSON, Generic)
