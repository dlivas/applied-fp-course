{-# LANGUAGE OverloadedStrings #-}
module Level05.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad                      (join)
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Except               ( MonadError(..) )

import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Bifunctor                     (first, second, bimap)
import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             ( Connection
                                                    , Query (fromQuery)
                                                    )
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level05.Types                      ( Comment
                                                    , CommentText
                                                    , Error (DBError)
                                                    , Topic
                                                    , fromDBComment
                                                    , getCommentText
                                                    , getTopic
                                                    , mkTopic
                                                    )

import           Level05.AppM                       ( AppM(..)
                                                    , liftIO
                                                    , liftEither
                                                    )

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
newtype FirstAppDB = FirstAppDB
  { dbConn  :: Connection
  }

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB
  :: FilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open fp
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

runDB
  :: (a -> Either Error b)
  -> IO a
  -> AppM b
runDB transformResult dbAction =
  -- This function is intended to abstract away the running of DB functions and
  -- the catching of any errors. As well as the process of running some
  -- processing function over those results.
  -- Move your use of DB.runDBAction to this function to avoid repeating
  -- yourself in the various DB functions.
  --
  -- types to be used for the impemenntation:
  --
  -- data Error = UnknownRoute
  --    | EmptyCommentText
  --    | EmptyTopic
  --    | DBError SQLiteResponse
  --    deriving (Eq, Show)
  --
  -- type DatabaseResponse a = Either SQLiteResponse a
  --
  -- Sql.runDBAction :: IO a -> IO (DatabaseResponse a)
  --
  let
    runnDBAction' =
      AppM $
        Sql.runDBAction dbAction
          >>= return . either (Left . DBError) transformResult
  in
    catchError
      runnDBAction'
      (AppM . return . Left)

getComments
  :: FirstAppDB
  -> Topic
  -> AppM [Comment]
getComments app topic =
  let
    sql = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
    dbAction =
      Sql.query
        (dbConn app)
        sql
        (Sql.Only (getTopic topic))
    transformResult = traverse fromDBComment
  in
    runDB transformResult dbAction

addCommentToTopic
  :: FirstAppDB
  -> Topic
  -> CommentText
  -> AppM ()
addCommentToTopic app topic commentText =
  let
    sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
    dbAction =
      do
        t <- getCurrentTime
        Right () <$
          Sql.execute
            (dbConn app)
            sql
            (getTopic topic, getCommentText commentText, t)
  in
    runDB id dbAction

getTopics
  :: FirstAppDB
  -> AppM [Topic]
getTopics app =
  let
    sql = "SELECT DISTINCT topic FROM comments"
    dbAction = Sql.query_ (dbConn app) sql
    transformResult = traverse $ mkTopic . Sql.fromOnly
  in
    runDB transformResult dbAction

deleteTopic
  :: FirstAppDB
  -> Topic
  -> AppM ()
deleteTopic app topic=
  let
    sql = "DELETE FROM comments WHERE topic = ?"
    dbAction =
      Right () <$ Sql.execute (dbConn app) sql (Sql.Only (getTopic topic))
  in
    runDB id dbAction

-- Go to 'src/Level05/Core.hs' next.
