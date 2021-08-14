{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}

-- https://stackoverflow.com/questions/56709365/how-to-handle-exception-within-a-servant-handler-monad
-- https://stackoverflow.com/questions/54763143/catching-io-exceptions-in-servant

-- handling exceptions: https://www.reddit.com/r/haskell/comments/3d5kdm/help_with_catching_exceptions/

module Server.API.AuthAPI where

import Servant
import Servant.API
import Server.API.Requests (SignupForm (..))
import Server.Config (App)
import Server.Database.Setup (runDb)
import Server.Database.Queries (saveUser)
import Control.Monad.IO.Class (liftIO)

type AuthAPI = "api" :> "auth" :> "signup" :> ReqBody '[JSON] SignupForm :> Post '[JSON] ()

authServer :: ServerT AuthAPI App
authServer = signupRequest

signupRequest :: SignupForm -> App ()
signupRequest s@SignupForm {..} =
  if password == repeatPassword
    then
      runDb $ saveUser s
    else
      throwError err400 { errBody = "Passwords do not match" }


