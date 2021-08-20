{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- https://stackoverflow.com/questions/56709365/how-to-handle-exception-within-a-servant-handler-monad
-- https://stackoverflow.com/questions/54763143/catching-io-exceptions-in-servant

-- handling exceptions: https://www.reddit.com/r/haskell/comments/3d5kdm/help_with_catching_exceptions/

module Server.API.AuthAPI where

import Control.Monad.IO.Class (liftIO)
import Crypto.Hash (SHA256 (SHA256), hashWith)
import Crypto.Random (MonadRandom (getRandomBytes))
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Esqueleto.Experimental (Key)
import Database.Esqueleto.Legacy (Entity (Entity))
import RIO
import Servant
import Servant.API
import Servant.Auth.Server (CookieSettings, FromJWT, JWTSettings, SetCookie, ToJWT, acceptLogin)
import Server.API.Requests (LoginForm (..), SignupForm (..), (!??))
import Server.Config (App)
import Server.Core.Password (hashPassword)
import Server.Core.Types (HashedPassword (..), Password (..), Salt (..))
import Server.Database.Model (User (..))
import Server.Database.Queries (getUserByUsernameAndPassword, saveUser)
import Server.Database.Setup (runDb)

type AuthAPI =
  "api" :> "auth" :> "signup" :> ReqBody '[JSON] SignupForm :> PostNoContent
    :<|> "api" :> "auth" :> "login"
      :> ReqBody '[JSON] LoginForm
      :> Post
           '[JSON]
           (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] AuthenticatedUser)

authServer :: CookieSettings -> JWTSettings -> ServerT AuthAPI App
authServer cs js = signupRequest :<|> loginRequest cs js

signupRequest :: SignupForm -> App NoContent
signupRequest s@SignupForm {..} =
  if password == repeatPassword
    then do
      (HashedPassword hp, salt) <- liftIO $ hashPassword (Password password)
      runDb $ saveUser (s {password = hp}) salt
      pure NoContent
    else throwError err400 {errBody = "Passwords do not match"}

loginRequest ::
  CookieSettings ->
  JWTSettings ->
  LoginForm ->
  App (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] AuthenticatedUser)
loginRequest cs js LoginForm {username = un, password = pw} = do
  Entity userId userEntity <- runDb (getUserByUsernameAndPassword un pw) !?? unAuthorizedError
  let aUser = AuthenticatedUser userId (userUsername userEntity) (userSurname userEntity)
  applyCookies <- liftIO (acceptLogin cs js aUser) !?? unAuthorizedError
  pure $ applyCookies aUser
  where
    unAuthorizedError = err401 {errBody = "Invalid Credentials"}

data AuthenticatedUser = AuthenticatedUser {auId :: Key User, firstname :: Text, surname :: Text}
  deriving (Show, Generic)

instance ToJSON AuthenticatedUser

instance FromJSON AuthenticatedUser

instance ToJWT AuthenticatedUser

instance FromJWT AuthenticatedUser
