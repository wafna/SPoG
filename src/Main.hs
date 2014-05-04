{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Control.Applicative
import Control.Exception(bracket, IOException)
import Data.Int

data User = User { userId :: Int64, userName :: String }
   deriving (Show)

data MessageT = MessageT Int64 Int64 String
   deriving (Show)

data Message = Message { messageId :: Int64, messageSender :: Int64, messageRecipients :: [Int64], messageContent :: String }
   deriving (Show)

-- This guy lets us get currval after inserting a record.
instance FromRow Int64 where
   fromRow = field

instance FromRow User where
  fromRow = User <$> field <*> field

instance FromRow MessageT where
   fromRow = MessageT <$> field <*> field <*> field

showAllUsers :: Connection -> IO ()
showAllUsers connection = do
   us :: [User] <- query_ connection "SELECT * FROM users"
   putStrLn $ "-- all users [" ++ show (length us) ++ "]"
   sequence_ $ fmap (putStrLn . show) $ zip [(1 :: Int) ..] us

testAffected :: Integral a => String -> a -> IO Int64 -> IO ()
testAffected msg expected action = do
   actual <- action
   if (fromIntegral expected /= actual) then fail msg else return ()

-- creates message and returns the new id.
createMessage :: Connection -> Int64 -> [Int64] -> String -> IO Int64
createMessage connection sender recipients content = withTransaction connection $ do
   testAffected ("Cannot createMessage" ++ content) (1 :: Int) $ execute connection "INSERT INTO messages (sender, content) VALUES (?, ?)" (sender, content)
   i <- query_ connection "SELECT currval(pg_get_serial_sequence('messages', 'id'))" >>= return . head
   testAffected ("message recipients") (length recipients) $ executeMany connection "INSERT INTO recipients VALUES (?, ?)" $ fmap (i,) recipients
   return i

-- inserts user and returns the new id.
createUser :: Connection -> String -> IO Int64
createUser connection name = withTransaction connection $ do
   testAffected ("Cannot createUser: " ++ name) (1 :: Int) $ execute connection "INSERT INTO users (name) VALUES (?)" [name]
   i  <- query_ connection "SELECT currval(pg_get_serial_sequence('users', 'id'))" >>= return . head
   putStrLn $ "id of " ++ name ++ ": " ++ show i
   return i

getSentMessages :: Connection -> Int64 -> IO [MessageT]
getSentMessages connection sender = do
   query connection "SELECT * FROM messages WHERE sender = ?" [sender]

wipeOut :: Connection -> IO ()
wipeOut connection = withTransaction connection $ do
   r <- execute_ connection "DELETE FROM recipients"
   m <- execute_ connection "DELETE FROM messages"
   u <- execute_ connection "DELETE FROM users"
   putStrLn $ concat ["-- wipeout\n  recipients: ", show r, "\n    messages: ", show m, "\n       users: ", show u]

withConnection :: ConnectInfo -> (Connection -> IO a) -> IO a
withConnection connectInfo f = bracket (connect connectInfo) close f

main :: IO ()
main = do
   let connectInfo = ConnectInfo "localhost" 5432 "spoguser" "imateapot" "spogdb"
   withConnection connectInfo $ \ connection -> do
      wipeOut connection
      -- putStrLn $ show $ postgreSQLConnectionString connectInfo
      showAllUsers connection
      --testAffected "insert bob" 1 $ execute connection "INSERT INTO users (name) VALUES (?)" ["bob" :: String]
      bob <- createUser connection "bob"
      carol <- createUser connection "carol"
      ted <- createUser connection "ted"
      alice <- createUser connection "alice"
      showAllUsers connection
      m1 <- createMessage connection bob [carol, ted] "bob -> (carol, ted)"
      putStrLn $ concat ["message id: ", show m1]
      ms <- getSentMessages connection bob
      putStrLn $ show ms
