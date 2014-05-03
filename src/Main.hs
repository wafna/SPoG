{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Control.Applicative
import Data.Int

data User = User { userId :: Int64, userName :: String }
   deriving (Show)

instance FromRow User where
  fromRow = User <$> field <*> field

main :: IO ()
main = do
   let connectInfo = ConnectInfo "localhost" 5432 "spoguser" "imateapot" "spogdb"
   connection <- connect connectInfo
   -- putStrLn $ show $ postgreSQLConnectionString connectInfo
   u :: [User] <- query_ connection "select * from users"
   putStrLn $ show u
   newUser <- execute connection "INSERT INTO users (name) VALUES (?)" ["bob" :: String]
   putStrLn $ show newUser
   u :: [User] <- query_ connection "select * from users"
   putStrLn $ show u
