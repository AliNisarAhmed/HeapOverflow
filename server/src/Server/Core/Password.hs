module Server.Core.Password where

import Control.Monad.Except (MonadIO)
import Crypto.Hash (SHA256 (SHA256), hashWith)
import Crypto.Random (getRandomBytes)
import Data.Text (Text)
import qualified Data.Text as T
import RIO
import Server.Core.Types

newSalt :: MonadIO m => m Salt
newSalt = do
  rnd <- liftIO $ getRandomBytes 32
  return $ Salt $ T.pack $ show $ hashWith SHA256 (rnd :: ByteString)

hashPasswordWithSalt :: Password -> Salt -> HashedPassword
hashPasswordWithSalt (Password password) (Salt salt) =
  let _salt = hashWith SHA256 (encodeUtf8 salt)
      _password = hashWith SHA256 _salt
   in HashedPassword $ T.pack $ show _password

hashPassword :: MonadIO m => Password -> m (HashedPassword, Salt)
hashPassword password = do
  salt <- newSalt
  let hash = hashPasswordWithSalt password salt
  pure (hash, salt)

verifyPassword :: Password -> Salt -> HashedPassword -> Bool
verifyPassword p s hp =
  let _hash = hashPasswordWithSalt p s
   in _hash == hp
