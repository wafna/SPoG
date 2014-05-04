{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Control.Applicative
import Control.Exception (bracket, IOException)
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map

data User = User { userId :: Int64, userName :: String }
   deriving (Show)

-- This maps directly onto the messages table and is used for storing intermediate results.
data MessageT = MessageT { messageIdT :: Int64, messageSenderT :: Int64, messageContentT :: String }
   deriving (Show)

data Message = Message { messageId :: Int64, messageSender :: Int64, messageRecipients :: [Int64], messageContent :: String }
   deriving (Show)

-- This guy lets us get currval after inserting a record.
instance FromRow Int64 where
   fromRow = field

data Int64_2 = Int64_2 Int64 Int64

-- 2-tuples of int64, e.g. from recipients table.
instance FromRow Int64_2 where
   fromRow = Int64_2 <$> field <*> field

instance FromRow User where
  fromRow = User <$> field <*> field

instance FromRow MessageT where
   fromRow = MessageT <$> field <*> field <*> field

showAllUsers :: Connection -> IO ()
showAllUsers connection = do
   us :: [User] <- query_ connection "SELECT * FROM users"
   putStrLn $ "-- all users [" ++ show (length us) ++ "]"
   sequence_ $ fmap (putStrLn . show) $ zip [(1 :: Int) ..] us

-- insists that we affected the number of rows we think we should have.
testAffected :: Integral a => String -> a -> IO Int64 -> IO ()
testAffected msg expected action = action >>= \ actual -> if (fromIntegral expected /= actual) then fail msg else return ()

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

{-
Some suboptimal choices, here.
1. Join messages to recipients. This gets us the answer in a single query but duplicates the message content for every recipient.
2. Select from messages and use that to form an in list to use on the recipients table. This requires two queries but less bandwidth.
3. Iterate the messages and query the recipients for each.  This is listed only for completeness and to note that it would fit better over the native data types
in that we wouldn't need intermediate types like MessageT to hold partial results.
-}
getSentMessages :: Connection -> Int64 -> IO [Message]
getSentMessages connection sender = do
   -- messages in "table" format (i.e. no dependent data like recipients)
   mts :: [MessageT] <- query connection "SELECT * FROM messages WHERE sender = ?" [sender]
   -- all the recipients for all the messages selected, above.
   ars :: [Int64_2] <- query connection "SELECT * FROM recipients WHERE message_id in ?" $ Only $ In $ fmap messageIdT mts
   -- map all the recipients under each message
   let m2rs = (foldl combineRecipients Map.empty ars)
   return $ fmap (makeMessage m2rs) mts
   where
   combineRecipients :: Map Int64 [Int64] -> Int64_2 -> Map Int64 [Int64]
   -- nb flip ++ here so that the singleton r get prepended to the list.
   combineRecipients mm (Int64_2 m r) = Map.insertWith (flip (++)) m [r] mm
   makeMessage m2rs mt = let mid = messageIdT mt in
      Message mid (messageSenderT mt) (Map.findWithDefault (error $ "no recipients for message " ++ show mid) mid m2rs) (messageContentT mt)

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
      bob <- createUser connection "bob"
      carol <- createUser connection "carol"
      ted <- createUser connection "ted"
      alice <- createUser connection "alice"
      showAllUsers connection
      createMessage connection bob [carol, ted] "bob -> (carol, ted)"
      createMessage connection bob [alice] "bob -> (alice)"
      createMessage connection alice [bob] "alice -> (bob)"
      ms <- getSentMessages connection bob
      sequence_ $ fmap (putStrLn . show) ms
