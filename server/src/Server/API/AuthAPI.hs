{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- https://stackoverflow.com/questions/56709365/how-to-handle-exception-within-a-servant-handler-monad
-- https://stackoverflow.com/questions/54763143/catching-io-exceptions-in-servant

-- handling exceptions: https://www.reddit.com/r/haskell/comments/3d5kdm/help_with_catching_exceptions/

module Server.API.AuthAPI where

import Control.Monad.IO.Class (liftIO)
import Crypto.Hash (SHA256 (SHA256), hashWith)
import Crypto.Random (MonadRandom (getRandomBytes))
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import RIO
import Servant
import Servant.API
import Server.API.Requests (SignupForm (..))
import Server.Config (App)
import Server.Database.Queries (saveUser)
import Server.Database.Setup (runDb)

type AuthAPI = "api" :> "auth" :> "signup" :> ReqBody '[JSON] SignupForm :> Post '[JSON] ()

authServer :: ServerT AuthAPI App
authServer = signupRequest

signupRequest :: SignupForm -> App ()
signupRequest s@SignupForm {..} =
  if password == repeatPassword
    then runDb $ saveUser s
    else throwError err400 {errBody = "Passwords do not match"}

newtype Salt = Salt {getSalt :: Text}
  deriving (Eq, Show, Read)

newSalt :: MonadIO m => m Salt
newSalt = do
  rnd <- liftIO $ getRandomBytes 32
  return $ Salt $ T.pack $ show $ hashWith SHA256 (rnd :: ByteString)
