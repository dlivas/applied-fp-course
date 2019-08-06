{-# LANGUAGE OverloadedStrings #-}
module Level02.Core (runApp, app) where

import           Network.Wai              ( Application
                                          , Request
                                          , Response
                                          , pathInfo
                                          , requestMethod
                                          , responseLBS
                                          , strictRequestBody
                                          )

import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       ( Status
                                          , hContentType
                                          , status200
                                          , status400
                                          , status404
                                          )

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              ( Either(..)
                                          , either
                                          )

import qualified Data.Text                as T

import           Data.Text.Encoding       ( decodeUtf8
                                          , encodeUtf8
                                          )

import           Level02.Types            ( ContentType(..)
                                          , Error(..)
                                          , RqType(..)
                                          , mkCommentText
                                          , mkTopic
                                          , getTopic
                                          , getCommentText
                                          , renderContentType
                                          )

-- |-------------------------------------------|
-- |- Don't start here, go to Level02.Types!  -|
-- |-------------------------------------------|

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse status contentType =
  responseLBS
    status
    [("Content-Type", renderContentType contentType)]

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 =
  mkResponse status200

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 =
  mkResponse status404

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 =
  mkResponse status400

-- |----------------------------------------------------------------------------------
-- These next few functions will take raw request information and construct         --
-- one of our types.                                                                --
--                                                                                  --
-- By breaking out these smaller functions, we're able to isolate our               --
-- validation requirements into smaller components that are simpler to maintain     --
-- and verify. It also allows for greater reuse and it also means that              --
-- validation is not duplicated across the application, maybe incorrectly.          --
--------------------------------------------------------------------------------------

mkAddRequest
  :: T.Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest topic comment =
  AddRq <$> mkTopic topic <*> mkCommentTextFromLBS comment
  where
    -- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
    mkCommentTextFromLBS =
      mkCommentText . decodeUtf8 . LBS.toStrict

mkViewRequest
  :: T.Text
  -> Either Error RqType
mkViewRequest =
  (ViewRq <$>) . mkTopic

mkListRequest
  :: Either Error RqType
mkListRequest =
  Right ListRq

mkUndefinedRequestRequest
  :: Either Error a
mkUndefinedRequestRequest =
  Left UndefinedRequest

-- |----------------------------------
-- end of RqType creation functions --
--------------------------------------

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse EmptyTopic =
  resp400  PlainText "Empty Topic"
mkErrorResponse EmptyCommentText =
  resp400 PlainText "Empty Comment Text"
mkErrorResponse UndefinedRequest =
  resp404 PlainText "Undefined Request"

-- | Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest
  :: Request
  -> IO (Either Error RqType)
mkRequest r =
  case (pathInfo r, requestMethod r) of
    ([t, "add"], "POST") ->
      mkAddRequest t <$> strictRequestBody r
    ([t, "view"], "GET") ->
      pure $ mkViewRequest t
    (["list"], "GET") ->
      pure mkListRequest
    _ ->
      pure mkUndefinedRequestRequest

-- | If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest
  :: RqType
  -> Either Error Response
handleRequest (AddRq topic comment) =
  Right $
    resp200
      PlainText
      (LBS.fromStrict . encodeUtf8 $
        T.concat
          [ "Added topic/comment: "
          , getTopic topic
          , " / "
          , getCommentText comment
          ]
        )
handleRequest (ViewRq topic) =
  Right $
    resp200
      PlainText
      (LBS.fromStrict . encodeUtf8 $
        T.concat
          [ "Viewed topic: "
          , getTopic topic
          ]
        )
handleRequest ListRq =
  Right $
    resp200
      PlainText
      "Listed Topics"

-- | Reimplement this function using the new functions and ``RqType`` constructors as a guide.
app
  :: Application
app request callback =
  do
    request' <- mkRequest request
    let response = either mkErrorResponse id (request' >>= handleRequest)
    callback response

runApp :: IO ()
runApp =
  do
    putStrLn $ "\n Servet accessible at http://localhost:3000/ \n"
    run 3000 app
