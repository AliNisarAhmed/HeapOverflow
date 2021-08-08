{-# LANGUAGE DeriveGeneric #-} 
{-# LANGUAGE DeriveAnyClass #-}

module Server.API.Requests where 

import Data.Aeson
import GHC.Generics

import Data.Text (Text)

data CreateQuestionRequest = CreateQuestionRequest 
    { title :: Text, 
      content :: Text, 
      userId :: Int 
    } deriving (Eq, Show, ToJSON, FromJSON, Generic)
