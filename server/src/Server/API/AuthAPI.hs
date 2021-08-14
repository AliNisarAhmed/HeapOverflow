{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Server.API.AuthAPI where

import Servant
import Servant.API
import Server.API.Requests (SignupForm (..))
import Server.Config (App)
import Server.Database.Setup (runDb)
import Server.Database.Queries (saveUser)
import Control.Exception (try)

type AuthAPI = "api" :> "auth" :> "signup" :> ReqBody '[JSON] SignupForm :> Post '[JSON] ()

authServer :: ServerT AuthAPI App
authServer = signupRequest

signupRequest :: SignupForm -> App ()
signupRequest s@SignupForm {..} =
  if password == repeatPassword
    then do
      runDb $ saveUser s 
    else
      throwError err400 { errBody = "Passwords do not match" }


