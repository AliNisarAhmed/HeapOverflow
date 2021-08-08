{-# LANGUAGE TypeApplications #-}


module Server.Database.Queries where

import Database.Esqueleto.Experimental (select, from, table, insertEntity)
import Server.Database.Model
import Database.Persist (Entity(..))
import Server.Database.Setup

getAllQuestions :: DbQuery [Entity Question]
getAllQuestions =
  select $ do
    from $ table @Question

createQuestion :: Question -> DbQuery (Entity Question) 
createQuestion = insertEntity

