module Main where

import Lib (app)
import Network.Wai.Handler.Warp
import RIO
import Server.Config (Config (..))
import Server.Database.Setup (connectDb, migrateDb)
import Prelude (putStrLn)

main :: IO ()
main = do
  putStrLn "starting the server"
  let connectionString = "host=localhost dbname=oopsoverflow user=postgres password=abc123"
  pool <- connectDb connectionString
  migrateDb pool
  let cfg = Config pool
  let port = 5000
  putStrLn $ "Server started on port " <> show port
  run port $ app cfg
