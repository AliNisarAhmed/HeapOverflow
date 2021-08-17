{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Server.Database.Queries where

import Control.Monad.Except (void)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Esqueleto.Experimental
import Database.Persist (Entity (..))
import qualified Database.Persist as P
import RIO hiding ((^.))
import Server.API.Requests (SignupForm (..))
import Server.Database.Model
import Server.Database.Setup

-- QUETIONS

getAllQuestions :: DbQuery [Entity Question]
getAllQuestions =
  select $ do
    from $ table @Question

createQuestion :: Question -> DbQuery (Entity Question)
createQuestion = insertEntity

getQuestionById :: Key Question -> DbQuery (Maybe Question)
getQuestionById = get

updateQuestion :: Key Question -> Text -> UTCTime -> DbQuery (Entity Question)
updateQuestion questionId updatedContent updatedAt = do
  q <- updateGet questionId [QuestionContent P.=. updatedContent, QuestionUpdatedAt P.=. updatedAt]
  pure $ Entity questionId q

-- ANSWERS

getAnswerById :: Key Answer -> DbQuery (Maybe Answer)
getAnswerById = get

getAnswersByQuestionId :: Key Question -> DbQuery [Entity Answer]
getAnswersByQuestionId questionId =
  select $ do
    answers <- from $ table @Answer
    where_ (answers ^. AnswerQuestionId ==. val questionId)
    pure answers

createAnswer :: Key Question -> Text -> Key User -> UTCTime -> DbQuery (Entity Answer)
createAnswer key articleContent answererId now =
  insertEntity (Answer key articleContent answererId now now)

updateAnswer :: Key Answer -> Text -> UTCTime -> DbQuery (Entity Answer)
updateAnswer answerId updatedContent updatedAt = do
  a <- updateGet answerId [AnswerContent P.=. updatedContent, AnswerUpdatedAt P.=. updatedAt]
  pure $ Entity answerId a

-- USER

saveUser :: SignupForm -> DbQuery ()
saveUser SignupForm {..} =
  void $ insert $ User firstname surname username email password
