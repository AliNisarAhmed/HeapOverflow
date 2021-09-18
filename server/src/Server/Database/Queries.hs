{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Server.Database.Queries where

import Control.Monad.Except (void)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Esqueleto hiding (from, on)
import Database.Esqueleto.Experimental
import Database.Persist (Entity (..))
import qualified Database.Persist as P
import RIO hiding (on, (^.))
import qualified RIO.Map as Map
import Server.API.Requests (SignupForm (..))
import Server.Core.Password (hashPasswordWithSalt)
import Server.Core.Types
import Server.Database.Model
import Server.Database.Setup

-- TAGS

createTags :: [Text] -> DbQuery [Entity Tag]
createTags tags = do
  putMany (fmap Tag tags)
  select $ do
    tag <- from $ table @Tag
    where_ $ tag ^. TagTitle `in_` valList tags
    pure tag

createQuestionTags :: [Key Tag] -> Key Question -> DbQuery ()
createQuestionTags tagIds questionId = void $ insertMany (fmap (QuestionTag questionId) tagIds)

-- QUESTIONS

getAllQuestions :: [Text] -> DbQuery [(Entity Question, Entity Tag)]
getAllQuestions tagFilters =
  select $ do
    (questions :& qTags :& tags) <-
      from $
        table @Question
          `InnerJoin` table @QuestionTag
          `on` ( \(qs :& qTags) ->
                   qs ^. QuestionId ==. qTags ^. QuestionTagQuestionId
               )
          `InnerJoin` table @Tag
          `on` ( \(_ :& qTags :& tags) ->
                   qTags ^. QuestionTagTagId ==. tags ^. TagId
               )
    unless (null tagFilters) $ where_ $ tags ^. TagTitle `in_` valList tagFilters
    pure (questions, tags)

createQuestion :: Question -> DbQuery (Entity Question)
createQuestion = insertEntity

getQuestionById :: Key Question -> DbQuery (Maybe Question)
getQuestionById = get

updateQuestion :: Key Question -> Text -> UTCTime -> DbQuery (Entity Question)
updateQuestion questionId updatedContent updatedAt = do
  q <- updateGet questionId [QuestionContent P.=. updatedContent, QuestionUpdatedAt P.=. updatedAt]
  pure $ Entity questionId q

deleteQuestionById :: Key Question -> DbQuery ()
deleteQuestionById = P.delete

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

deleteAnswerById :: Key Answer -> DbQuery ()
deleteAnswerById = P.delete

-- USER

saveUser :: SignupForm -> Salt -> DbQuery ()
saveUser SignupForm {..} (Salt s) =
  void $ insert $ User firstname surname username email password s

getUserByUsername :: Text -> DbQuery (Maybe (Entity User))
getUserByUsername = getBy . UniqueUsername

getUserByUsernameAndPassword :: Text -> Text -> DbQuery (Maybe (Entity User))
getUserByUsernameAndPassword un pw = do
  mu <- getUserByUsername un
  case mu of
    Nothing -> pure Nothing
    Just u@(Entity _ (User fn _ sn em hashedPwd salt)) ->
      if verifyPassword (HashedPassword hashedPwd) (Password pw) (Salt salt)
        then pure $ Just u
        else pure Nothing
  where
    verifyPassword :: HashedPassword -> Password -> Salt -> Bool
    verifyPassword hashedPwd pwd salt =
      let HashedPassword h = hashPasswordWithSalt pwd salt
       in h == getHashedPassword hashedPwd
