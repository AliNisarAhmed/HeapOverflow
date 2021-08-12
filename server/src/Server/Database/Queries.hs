{-# LANGUAGE TypeApplications #-}

module Server.Database.Queries where

import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Esqueleto.Experimental
import Database.Persist (Entity (..))
import Server.Database.Model
import Server.Database.Setup

getAllQuestions :: DbQuery [Entity Question]
getAllQuestions =
  select $ do
    from $ table @Question

createQuestion :: Question -> DbQuery (Entity Question)
createQuestion = insertEntity

getQuestionById :: Key Question -> DbQuery (Maybe Question)
getQuestionById = get

-- ANSWERS

getAnswersByQuestionId :: Key Question -> DbQuery [Entity Answer]
getAnswersByQuestionId questionId =
  select $ do
    answers <- from $ table @Answer
    where_ (answers ^. AnswerQuestionId ==. val questionId)
    pure answers

createAnswer :: Key Question -> Text -> Int -> UTCTime -> DbQuery (Entity Answer)
createAnswer key articleContent answererId now =
  insertEntity (Answer key articleContent answererId now now)
