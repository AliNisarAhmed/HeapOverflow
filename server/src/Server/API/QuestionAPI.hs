{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server.API.QuestionAPI (questionServer, QuestionApi) where

import Server.Config (App (..))
import Server.Database.Setup (runDb)
import Database.Persist (Entity (..), selectList)
import Server.Database.Model
import Servant

type QuestionApi =
  "api" :> "whoami" :> Get '[JSON] [Entity Question]

questionServer :: ServerT QuestionApi App
questionServer = whoami

whoami :: App [Entity Question]
whoami = runDb $ selectList [] []