{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server.API.QuestionAPI (questionServer, QuestionApi) where

import Server.Config (App (..))
import Server.Database.Setup (runDb)
import Database.Persist (Entity (..), selectList)
import Server.Database.Model
import Servant
import Database.Persist (Entity(..), selectList, insertEntity)
import Server.Database.Setup (runDb, DbQuery)
import Database.Esqueleto (select, from) 

type QuestionApi =
    "api" :> "questions" :> 
  ( 
    Get '[JSON] [Entity Question]
  )

questionServer :: ServerT QuestionApi App
questionServer = getQuestions

getQuestions :: App [Entity Question] 
getQuestions = runDb getAllQuestions


getAllQuestions :: DbQuery [Entity Question]
getAllQuestions = 
  select $ from $ \q -> return q
