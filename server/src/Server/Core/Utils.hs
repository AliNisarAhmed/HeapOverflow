module Server.Core.Utils where 

-- https://www.reddit.com/r/haskelltil/comments/3mj2ib/convenience_functions_for_reading_environment/

import RIO
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import System.Environment (lookupEnv, getEnv)
import RIO.Partial (read)

fromEnv :: (Read a) => a -> String -> IO a
fromEnv d = fmap (maybe d read) . lookupEnv

readEnv :: Text -> IO ByteString
readEnv = fmap B.pack . getEnv . T.unpack
