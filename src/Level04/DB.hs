{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Level04.DB
  ( FirstAppDB(FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  )
where

import           Control.Monad                      (join)

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text

import           Data.Time                      ( getCurrentTime )

import           Data.Bifunctor                 ( first
                                                , second
                                                , bimap
                                                )

import           Database.SQLite.Simple         ( Connection
                                                , Query(Query)
                                                )
import qualified Database.SQLite.Simple        as Sql

import qualified Database.SQLite.SimpleErrors  as Sql
import           Database.SQLite.SimpleErrors.Types
                                                ( SQLiteResponse )

import           Level04.Types                  ( Comment
                                                , CommentText
                                                , mkTopic
                                                , mkCommentText
                                                , getTopic
                                                , getCommentText
                                                , fromDBComment
                                                , Error(..)
                                                , Topic(..)
                                                )

-- ------------------------------------------------------------------------|
-- You'll need the documentation for sqlite-simple ready for this section! |
-- ------------------------------------------------------------------------|

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
--
-- To help with that, we create a new data type that can hold our `Connection`
-- for us, and allows it to be expanded later if we need to
newtype FirstAppDB = FirstAppDB
  { dbConn :: Connection
  }

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB -> IO ()
closeDB = Sql.close . dbConn

-- Given a `FilePath` to our SQLite DB file, initialise the database and ensure
-- our Table is there by running a query to create it, if it doesn't exist
-- already.
initDB
  :: FilePath
  -> IO (Either SQLiteResponse FirstAppDB)
initDB =
  let
    dbAction fp =
      do
        connection <- Sql.open fp
        Sql.execute_ connection createTableQ
        return $ FirstAppDB connection
  in
    Sql.runDBAction . dbAction
 where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
  createTableQ
    = "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time TEXT)"

-- Note that we don't store the `Comment` in the DB, it is the type we build
-- to send to the outside world. We will be loading our `DBComment` type from
-- the FirstApp.DB.Types module before converting trying to convert it to a
-- `Comment`.
--
-- To go from a DBComment to a Comment, we need to use ``fromDBComment`` that is
-- defined in FirstApp.Types.
--
-- HINT: You can use '?' or named place-holders as query parameters. Have a look
-- at the section on parameter substitution in sqlite-simple's documentation.
-- There are several possible implementations of this function. Particularly
-- there may be a trade-off between deciding to throw an Error if a DBComment
-- cannot be converted to a Comment, or simply ignoring any DBComment that is
-- not valid.
getComments
  :: FirstAppDB
  -> Topic
  -> IO (Either Error [Comment])
getComments app (Topic topic) =
  let
    sql = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
    conn = dbConn app
    dbAction = Sql.query conn sql (Sql.Only topic)
  in
    do
      -- type DatabaseResponse a = Either SQLiteResponse a
      -- (Sql.runDBAction dbAction) ::
      --    IO (DatabaseResponse [DBComment])
      -- fromDBComment :: DBComment -> Either Error Comment
      -- dbResult :: DatabaseResponse [DBComment]
      dbResult <- Sql.runDBAction dbAction
      return $ first DBError dbResult >>= traverse fromDBComment

addCommentToTopic
  :: FirstAppDB
  -> Topic
  -> CommentText
  -> IO (Either Error ())
addCommentToTopic app topic comment =
  let
    sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
    conn = dbConn app
    dbAction t =
      Sql.execute conn sql (getTopic topic, getCommentText comment, t)
  in
    do
      -- type DatabaseResponse a = Either SQLiteResponse a
      -- (Sql.runDBAction (dbAction t)) ::
      --    IO (DatabaseResponse ())
      t <- getCurrentTime
      first DBError <$> Sql.runDBAction (dbAction t)

getTopics
  :: FirstAppDB
  -> IO (Either Error [Topic])
getTopics app =
  let
    sql = "SELECT DISTINCT topic FROM comments"
    conn = dbConn app
    dbAction = Sql.query_ conn sql
  in
    do
      -- type DatabaseResponse a = Either SQLiteResponse a
      -- (Sql.runDBAction dbAction) ::
      --    IO (DatabaseResponse [Text])
      -- mkTopic :: Text -> Either Error Topic
      -- dbResult :: DatabaseResponse [Text]
      dbResult <- Sql.runDBAction dbAction
      return $
        first DBError dbResult
          >>= traverse (mkTopic . Sql.fromOnly)

deleteTopic
  :: FirstAppDB
  -> Topic
  -> IO (Either Error ())
deleteTopic app (Topic topic) =
  let
    sql = "DELETE FROM comments WHERE topic = ?"
    conn = dbConn app
    dbAction = Sql.execute conn sql (Sql.Only topic)
  in
    -- type DatabaseResponse a = Either SQLiteResponse a
    -- (Sql.runDBAction dbAction) ::
    --    IO (DatabaseResponse ())
    first DBError <$> Sql.runDBAction dbAction
