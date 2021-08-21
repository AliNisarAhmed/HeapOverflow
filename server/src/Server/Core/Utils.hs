module Server.Core.Utils where 

import RIO
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import System.Environment (lookupEnv, getEnv)
import RIO.Partial (read)

fromEnv :: (Read a) => a -> String -> IO a
fromEnv d = fmap (maybe d read) . lookupEnv

readEnv :: Text -> IO ByteString
readEnv = fmap B.pack . getEnv . T.unpack
