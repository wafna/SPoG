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

-- stuff for Field marshaling
import Database.PostgreSQL.Simple.FromField
      ( FromField (fromField) , typeOid, returnError, ResultError (..) )
import Database.PostgreSQL.Simple.ToField 
      (ToField (toField))
import Database.PostgreSQL.Simple.TypeInfo.Static (typoid, int8)
import qualified Data.ByteString.Char8 as B
import Data.Typeable()
import Data.Data

data User = User { userId :: UserId, userName :: String }
   deriving (Show)

newtype UserId = UserId { _uid :: Int64 }
  deriving (Eq, Ord, Show, Typeable)

instance FromField UserId where
   fromField f mdata = 
      if typeOid f /= typoid int8
        then returnError Incompatible f ""
        else case B.unpack `fmap` mdata of
            Nothing  -> returnError UnexpectedNull f ""
            Just dat -> return $ UserId $ read dat

instance ToField UserId where
   toField (UserId i) = toField i

-- This maps directly onto the messages table and is used for storing intermediate results.
data MessageT = MessageT { messageIdT :: MessageId, messageSenderT :: UserId, messageContentT :: String }
   deriving (Show)

-- The hierarchical message type (has recipients).
data Message = Message { messageId :: MessageId, messageSender :: UserId, messageRecipients :: [UserId], messageContent :: String }
   deriving (Show)

newtype MessageId = MessageId { _mid :: Int64 }
  deriving (Eq, Ord, Show, Typeable)

instance FromField MessageId where
   fromField f mdata = 
      if typeOid f /= typoid int8
        then returnError Incompatible f ""
        else case B.unpack `fmap` mdata of
            Nothing  -> returnError UnexpectedNull f ""
            Just dat -> return $ MessageId $ read dat

instance ToField MessageId where
   toField (MessageId i) = toField i

-- This guy lets us get currval after inserting a record.
instance FromRow Int64 where
   fromRow = field

instance FromRow MessageId where
   fromRow = field

instance FromRow UserId where
   fromRow = field

-- Rows from the recipients table.
data MessageRecipient = MessageRecipient MessageId UserId

instance FromRow MessageRecipient where
   fromRow = MessageRecipient <$> field <*> field

instance FromRow User where
  fromRow = User <$> field <*> field

instance FromRow MessageT where
   fromRow = MessageT <$> field <*> field <*> field

-- insists that we affected the number of rows we think we should have.
testAffected :: Integral a => String -> a -> IO Int64 -> IO ()
testAffected msg expected action = action >>= \ actual -> if (fromIntegral expected /= actual) then fail msg else return ()

-- inserts user and returns the new id.
-- transaction probably not necessary here since user has no dependent records and currval is contextualized to the session.
createUser :: Connection -> String -> IO UserId
createUser connection name = withTransaction connection $ do
   testAffected ("Cannot createUser: " ++ name) (1 :: Int) $ execute connection "INSERT INTO users (name) VALUES (?)" [name]
   i  <- query_ connection "SELECT currval(pg_get_serial_sequence('users', 'id'))" >>= return . head
   putStrLn $ "id of " ++ name ++ ": " ++ show i
   return $ UserId i

justOne :: IO [a] -> IO (Maybe a)
justOne q = q >>= \ xs -> return $ case xs of
   [] -> Nothing
   x : [] -> Just x
   _ -> error "Too many!"

getUserById :: Connection -> UserId -> IO (Maybe User)
getUserById connection uid = justOne $ query connection "SELECT * FROM users WHERE id = ?" $ Only uid

getUserByName :: Connection -> String -> IO (Maybe User)
getUserByName connection name = justOne $ query connection "SELECT * FROM users WHERE name = ?" $ Only name

-- creates message and returns the new id.
createMessage :: Connection -> UserId -> [UserId] -> String -> IO MessageId
createMessage connection sender recipients content = withTransaction connection $ do
   testAffected ("Cannot createMessage" ++ content) (1 :: Int) $ execute connection "INSERT INTO messages (sender, content) VALUES (?, ?)" (sender, content)
   i <- query_ connection "SELECT currval(pg_get_serial_sequence('messages', 'id'))" >>= return . head
   testAffected ("message recipients") (length recipients) $ executeMany connection "INSERT INTO recipients VALUES (?, ?)" $ fmap (i,) recipients
   return i

{-
Some suboptimal choices, here.
1. Join messages to recipients. This gets us the answer in a single query but duplicates the message content for every recipient.
2. Select from messages and use that to form an in list to use on the recipients table. This requires two queries but less bandwidth.
3. Iterate the messages and query the recipients for each.  This is listed only for completeness and to note that it would fit better over the native data types
in that we wouldn't need intermediate types like MessageT to hold partial results.
-}
getSentMessages :: Connection -> UserId -> IO [Message]
getSentMessages connection sender = withTransaction connection $ do
   -- messages in "table" format (i.e. no dependent data like recipients)
   query connection "SELECT * FROM messages WHERE sender = ?" [_uid sender] >>= getMessages connection

getReceivedMessages :: Connection -> UserId -> IO [Message]
getReceivedMessages connection receiver = withTransaction connection $ do
   mids :: [Int64] <- query connection "SELECT message_id FROM recipients WHERE recipient_id = ?" $ Only $ _uid receiver
   (query connection "SELECT * FROM messages WHERE id in ?" $ Only $ In mids) >>= getMessages connection

-- takes records from the messages table and turns them into hierarchical message records (i.e. with recipients).
-- this function does not initiate a transaction.
getMessages :: Connection -> [MessageT] -> IO [Message]
getMessages connection mts = do
   -- all the recipients for all the message table records.
   ars :: [MessageRecipient] <- query connection "SELECT * FROM recipients WHERE message_id in ?" $ Only $ In $ fmap messageIdT mts
   -- map all the recipients under each message
   let m2rs = (foldl combineRecipients Map.empty ars)
   return $ fmap (makeMessage m2rs) mts
   where
   combineRecipients :: Map MessageId [UserId] -> MessageRecipient -> Map MessageId [UserId]
   -- nb flip ++ here so that the singleton r get prepended to the list (for efficiency).
   combineRecipients mm (MessageRecipient m r) = Map.insertWith (flip (++)) m [r] mm
   makeMessage :: Map MessageId [UserId] -> MessageT -> Message
   makeMessage m2rs mt = let mid = messageIdT mt in
      Message mid (messageSenderT mt) (Map.findWithDefault [] mid m2rs) (messageContentT mt)

-- wipes the database clean
wipeOut :: Connection -> IO ()
wipeOut connection = withTransaction connection $ do
   r <- execute_ connection "DELETE FROM recipients"
   m <- execute_ connection "DELETE FROM messages"
   u <- execute_ connection "DELETE FROM users"
   putStrLn $ concat ["-- wipeout\n  recipients: ", show r, "\n    messages: ", show m, "\n       users: ", show u]

withConnection :: ConnectInfo -> (Connection -> IO a) -> IO a
withConnection connectInfo f = bracket (connect connectInfo) close f

showAllUsers :: Connection -> IO ()
showAllUsers connection = do
   us :: [User] <- query_ connection "SELECT * FROM users"
   printList "all users" us

printList :: (Show a) => String -> [a] -> IO ()
printList header list = do
   putStrLn $ concat ["-- ", header, " [", show $ length list, "]"]
   sequence_ $ fmap printItem $ zip [(1 :: Int) ..] list
   where
   printItem :: (Show a) => (Int, a) -> IO ()
   printItem (n, i) = putStrLn $ concat ["  ", show n, ". ", show i]

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
      getUserById connection bob >>= putStrLn . show
      getUserByName connection "bob" >>= putStrLn . show
      createMessage connection bob [carol, ted] "bob -> [carol, ted]"
      createMessage connection bob [carol] "bob -> [carol]"
      createMessage connection bob [alice] "bob -> [alice]"
      createMessage connection alice [bob] "alice -> [bob]"
      createMessage connection ted [carol, alice] "ted -> [carol, alice]"
      ms <- getSentMessages connection bob
      printList "sent by bob" ms
      ms <- getReceivedMessages connection carol
      printList "received by carol" ms
