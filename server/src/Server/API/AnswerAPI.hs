{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}


module Server.API.AnswerAPI where 

import Database.Esqueleto.Experimental (Key)
import Server.Database.Model
import Server.Config (App)
import Database.Persist (Entity(Entity))
import Servant (ServerT, Get, JSON, type (:>), Capture)
import Server.Database.Setup (runDb)
import Server.Database.Queries (getAnswersByQuestionId)


type AnswerAPI = 
  "api" :> "questions" :> Capture "questionId" (Key Question) :> "answers" :> 
  (
    Get '[JSON] [Entity Answer]
  )
  

answerServer :: ServerT AnswerAPI App 
answerServer = getAnswersForQuestion 

getAnswersForQuestion :: Key Question -> App [Entity Answer] 
getAnswersForQuestion questionId = 
  runDb $ getAnswersByQuestionId questionId 
