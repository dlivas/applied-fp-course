{-# LANGUAGE OverloadedStrings #-}

module Level08.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Lens                       hiding ( element )
import           Control.Lens.TH

import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader               -- (asks)

import           Data.Bifunctor                     (first)
import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             ( Connection
                                                    , FromRow
                                                    , Query (fromQuery)
                                                    , ToRow
                                                    )

import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level08.AppM                       ( App
                                                    , AppM (..)
                                                    , Env (envDB)
                                                    , liftEither
                                                    )

import           Level08.Types                      ( Comment
                                                    , CommentText
                                                    , DBFilePath
                                                    , Error (DBError)
                                                    , FirstAppDB (FirstAppDB, dbConn)
                                                    , Topic
                                                    , fromDBComment
                                                    , getCommentText
                                                    , getTopic
                                                    , mkTopic
                                                    )

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

getDBConn ::
  App Connection
getDBConn =
  asks $ dbConn . envDB
  -- previous implementation:
  -- AppM $ return . Right . dbConn . envDB

runDB ::
  (a -> Either Error b)
  -> (Connection -> IO a)
  -> App b
runDB f cf =
  getDBConn
  >>= liftIO . cf
  >>= liftEither . f
  -- previous implementation:
  -- AppM $ fmap f . cf . dbConn . envDB

getComments
  :: Topic
  -> App [Comment]
getComments t = 
  runDB
    (traverse fromDBComment)
    $ \conn -> Sql.query conn q (Sql.Only . getTopic $ t)
  where
    -- Write the query with an icky string and remember your placeholders!
    -- To be doubly and triply sure we've no garbage in our response, we take care
    -- to convert our DB storage type into something we're going to share with the
    -- outside world. Checking again for things like empty Topic or CommentText values.
    q = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"

addCommentToTopic
  :: Topic
  -> CommentText
  -> App ()
addCommentToTopic t c = do
  -- Record the time this comment was created.
  nowish <- liftIO getCurrentTime
  -- We use the execute function this time as we don't care about anything
  -- that is returned. The execute function will still return the number of rows
  -- affected by the query, which in our case should always be 1.
  runDB
    Right
    $ \conn -> Sql.execute conn q (getTopic t, getCommentText c, nowish)
  -- An alternative is to write a returning query to get the Id of the DBComment
  -- we've created. We're being lazy (hah!) for now, so assume awesome and move on.
  where
    -- Note the triple, matching the number of values we're trying to insert, plus
    -- one for the table name.
    -- Remember that the '?' are order dependent so if you get your input
    -- parameters in the wrong order, the types won't save you here. More on that
    -- sort of goodness later.
    q = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"

getTopics
  :: App [Topic]
getTopics =
  runDB
    (traverse ( mkTopic . Sql.fromOnly ))
    $ \conn -> Sql.query_ conn q
  where
    q = "SELECT DISTINCT topic FROM comments"

deleteTopic
  :: Topic
  -> App ()
deleteTopic t =
  runDB
    Right
    $ \conn -> Sql.execute conn q (Sql.Only . getTopic $ t)
  where
    q = "DELETE FROM comments WHERE topic = ?"

-- Go on to 'src/Level07/Core.hs' next.
