{-# LANGUAGE TypeApplications #-}


module Server.Database.Queries where

import Database.Esqueleto.Experimental
import Server.Database.Model
import Database.Persist (Entity(..))
import Server.Database.Setup

getAllQuestions :: DbQuery [Entity Question]
getAllQuestions =
  select $ do
    from $ table @Question

createQuestion :: Question -> DbQuery (Entity Question) 
createQuestion = insertEntity


-- ANSWERS 

getAnswersByQuestionId :: Key Question -> DbQuery [Entity Answer] 
getAnswersByQuestionId questionId = 
  select $ do
    answers <- from $ table @Answer 
    where_ (answers ^. AnswerQuestionId ==. val questionId)
    pure answers
