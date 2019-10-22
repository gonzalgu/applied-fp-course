{-# LANGUAGE OverloadedStrings #-}
module Level02.Core 
-- (runApp, app) 
where

import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody, queryString, rawQueryString)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404, StdMethod(GET, POST), renderQuery)

import           Network.HTTP.Types.Method                                            

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8,encodeUtf8)

import           Level02.Types            (ContentType(Plain,Json), Error(Err), RqType(AddRq, ViewRq, ListRq),
                                           mkCommentText, mkTopic,
                                           renderContentType)

-- |-------------------------------------------|
-- |- Don't start here, go to Level02.Types!  -|
-- |-------------------------------------------|

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse st ct s =
  case fromEnum st of
    200 -> resp200 ct s 
    404 -> resp404 ct s
    400 -> resp400 ct s
    _   -> error "unhandled status"

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 ct s = responseLBS status200 [(hContentType, renderContentType ct)] s 
--  error "resp200 not implemented"

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 ct s = responseLBS status404 [(hContentType, renderContentType ct)] s
--   error "resp404 not implemented"

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 ct s = responseLBS status400 [(hContentType, renderContentType ct)] s
--  error "resp400 not implemented"

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
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest top comm = do
  topic <- mkTopic top 
  return $ AddRq topic (lazyByteStringToStrictText comm)
  where
    -- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
    lazyByteStringToStrictText =
      decodeUtf8 . LBS.toStrict

mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest topicText = do
  topic <- mkTopic topicText
  return $ ViewRq topic

mkListRequest
  :: Either Error RqType
mkListRequest = Right ListRq


-- |----------------------------------
-- end of RqType creation functions --
--------------------------------------

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse (Err err) = responseLBS status404 [] (textToLBString err)
  where textToLBString = LBS.fromStrict . encodeUtf8

-- | Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.

{-
# To comment on a given <topic>
POST /<topic>/add

# To view all the comments on a given <topic>
GET /<topic>/view

# To list all the current topics
GET /list
-}

mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest req = 
  case (parseMethod $ requestMethod req, pathInfo req) of 
    (Right POST, [topic,"add"] )  ->  return $ mkAddRequest topic "" --(rawQueryString req)     

    (Right GET,  [topic, "view"])  -> return $ mkViewRequest topic
    (Right GET,  ["list"]      )  -> return  $ mkListRequest
    
    _          -> return  $ Left $ Err "non recognized method."
 
{-
  do
  putStrLn $ show req
  return $ Left $ Err "not implemented"
-}
  -- Remembering your pattern-matching skills will let you implement the entire
  -- specification in this function.
  --error "mkRequest not implemented"

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
handleRequest _ = Right $ mkResponse status404 Plain "not implemented yet."
  --error "handleRequest not implemented"

-- | Reimplement this function using the new functions and ``RqType`` constructors as a guide.
-- Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app
  :: Application
app request handler = do
  rq <- mkRequest request
  case rq of
    Right r -> 
      case handleRequest r of
        Right resp -> handler resp
        Left err    -> handler $ mkErrorResponse err
    Left err -> handler $ mkErrorResponse err
        
  --return rq >>= handleRequest




  --error "app not reimplemented"

runApp :: IO ()
runApp = run 3000 app
