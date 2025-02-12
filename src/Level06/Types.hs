{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE NamedFieldPuns             #-}
module Level06.Types
  ( Error (..)
  , ConfigError (..)
  , PartialConf (..)
  , Port (..)
  , DBFilePath (..)
  , Conf (..)
  , RqType (..)
  , ContentType (..)
  , Comment (..)
  , Topic
  , CommentText
  , mkTopic
  , getTopic
  , encodeTopic
  , mkCommentText
  , getCommentText
  , encodeComment
  , renderContentType
  , confPortToWai
  , fromDBComment
  , partialConfDecoder
  ) where

import           GHC.Word                           (Word16)

import           Data.ByteString                    (ByteString)
import           Data.Text                          (Text, pack)

import           System.IO.Error                    (IOError)

import           Data.Semigroup                     (Last (..), Semigroup ((<>)))

import           Data.Functor.Contravariant         ((>$<))
import           Data.List                          (stripPrefix)
import           Data.Maybe                         (fromMaybe)
import           Data.Time                          (UTCTime)
import qualified Data.Time.Format                   as TF

import           System.Locale                      (defaultTimeLocale)

import           Waargonaut.Decode                  (Decoder)
import qualified Waargonaut.Decode                  as D
import           Waargonaut.Decode.Error            (DecodeError)

import           Waargonaut.Encode                  (Encoder)
import qualified Waargonaut.Encode                  as E

import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level06.DB.Types                   (DBComment (..))
import           Level06.Types.CommentText          (CommentText,
                                                     encodeCommentText,
                                                     getCommentText,
                                                     mkCommentText)

import           Level06.Types.Error                (Error (..))
import           Level06.Types.Topic                (Topic, encodeTopic,
                                                     getTopic, mkTopic)

newtype CommentId = CommentId Int
  deriving Show

encodeCommentId :: Applicative f => Encoder f CommentId
encodeCommentId = (\(CommentId i) -> i) >$< E.int

data Comment = Comment
  { commentId    :: CommentId
  , commentTopic :: Topic
  , commentText  :: CommentText
  , commentTime  :: UTCTime
  }
  deriving (Show)

encodeISO8601DateTime :: Applicative f => Encoder f UTCTime
encodeISO8601DateTime = E.encodeA $ E.runEncoder E.text . pack . TF.formatTime tl fmt
  where
    fmt = TF.iso8601DateFormat (Just "%H:%M:%S")
    tl = TF.defaultTimeLocale { TF.knownTimeZones = [] }

encodeComment :: Applicative f => Encoder f Comment
encodeComment = E.mapLikeObj $ \c ->
  E.atKey' "id"    encodeCommentId       (commentId c) .
  E.atKey' "topic" encodeTopic           (commentTopic c) .
  E.atKey' "text"  encodeCommentText     (commentText c) .
  E.atKey' "time"  encodeISO8601DateTime (commentTime c)

-- For safety we take our stored DBComment and try to construct a Comment that
-- we would be okay with showing someone. However unlikely it may be, this is a
-- nice method for separating out the back and front end of a web app and
-- providing greater guarantees about data cleanliness.
fromDBComment
  :: DBComment
  -> Either Error Comment
fromDBComment dbc =
  Comment (CommentId     $ dbCommentId dbc)
      <$> (mkTopic       $ dbCommentTopic dbc)
      <*> (mkCommentText $ dbCommentComment dbc)
      <*> (pure          $ dbCommentTime dbc)

-- We have to be able to:
-- - Comment on a given topic
-- - View a topic and its comments
-- - List the current topics
--
-- To that end, we have the following types:
--
-- AddRq : Which needs to the target topic, and the body of the comment.
-- ViewRq : Which needs the topic being requested.
-- ListRq : Which lists all of the current topics.
data RqType
  = AddRq Topic CommentText
  | ViewRq Topic
  | ListRq

data ContentType
  = PlainText
  | JSON

renderContentType
  :: ContentType
  -> ByteString
renderContentType PlainText = "text/plain"
renderContentType JSON      = "application/json"

-----------------
-- Config Types
-----------------

-- This is an alternative way of defining a `newtype`. You define it as a record
-- with a single field, this provides the unwrapping function for free. When
-- defined using the other method, you must use pattern-matching or write a dedicated
-- function in order to get the value out.
--
newtype Port = Port
  -- You will notice we're using ``Word16`` as our type for the ``Port`` value.
  -- This is because a valid port number can only be a 16bit unsigned integer.
  { getPort :: Word16 }
  deriving (Eq, Show)

newtype DBFilePath = DBFilePath
  { getDBFilePath :: FilePath }
  deriving (Eq, Show)

-- Add some fields to the ``Conf`` type:
-- - A customisable port number: ``Port``
-- - A filepath for our SQLite database: ``DBFilePath``
data Conf = Conf{ port :: Port, filePath :: DBFilePath} deriving Show

-- We're storing our Port as a Word16 to be more precise and prevent invalid
-- values from being used in our application. However Wai is not so stringent.
-- To accommodate this and make our lives a bit easier, we will write this
-- helper function to take ``Conf`` value and convert it to an ``Int``.
--
-- We'll need to use a function called; ``fromIntegral``, to convert our
-- ``Word16`` to an ``Int``. The type of this function is:
--
-- fromIntegral :: (Num b, Integral a) => a -> b
--
confPortToWai
  :: Conf
  -> Int
confPortToWai = fromIntegral . getPort . port
  --error "confPortToWai not implemented"

-- Similar to when we were considering our application types. We can add to this sum type
-- as we build our application and the compiler can help us out.
data ConfigError
  = BadConfFile DecodeError
  | FileNotFound Text
  | UndefinedProperty Text
  deriving Show

-- Our application will be able to load configuration from both a file and
-- command line input. We want to be able to use the command line to temporarily
-- override the configuration from our file. How do we combine the different
-- inputs to enable this property?

-- We want the command line configuration to take precedence over the File
-- configuration, so if we think about combining each of our ``Conf`` records,
-- we want to be able to write something like this:

-- ``defaults <> file <> commandLine``

-- We can use the ``Semigroup`` typeclass to handle combining the ``Conf`` records
-- together, and the ``Last`` type to wrap up our values to handle the desired
-- precedence. The ``Last`` type is a wrapper for Maybe that when used with its
-- ``Semigroup`` instance will always preference the last value that it has:
--
-- Just (Last 3) <> Just (Last 1) = Just (Last 1)
-- Nothing       <> Just (Last 1) = Just (Last 1)
-- Just (Last 1) <> Nothing       = Just (Last 1)
--
-- To make this easier, we'll make a new type ``PartialConf`` that will have our ``Last``
-- wrapped values. We can then define a ``Semigroup`` instance for it and have our
-- ``Conf`` be a known good configuration.
data PartialConf = PartialConf
  { pcPort       :: Maybe (Last Port)
  , pcDBFilePath :: Maybe (Last DBFilePath)
  }
  deriving(Show)

-- We need to define a ``Semigroup`` instance for ``PartialConf``. We define our ``(<>)``
-- function to lean on the ``Semigroup`` instance for Last to always get the last value.
instance Semigroup PartialConf where
  _a <> _b = PartialConf
    { pcPort       = (pcPort _a) <> (pcPort _b)
    , pcDBFilePath = (pcDBFilePath _a) <> (pcDBFilePath _b)
    }

-- | When it comes to reading the configuration options from the command-line, we
-- use the 'optparse-applicative' package. This part of the exercise has already
-- been completed for you, feel free to have a look through the 'CommandLine'
-- module and see how it works.
--
-- For reading the configuration from the file, we're going to use the Waargonaut
-- library to handle the parsing and decoding for us. In order to do this, we
-- have to tell waargonaut how to go about converting the JSON into our PartialConf
-- data structure.
partialConfDecoder :: Monad f => Decoder f PartialConf
partialConfDecoder = PartialConf
  <$> D.atKey "port" getPort
  <*> D.atKey "path" getPath
  where
    getPort = Just . Last . Port <$>  D.integral
    getPath = Just . Last . DBFilePath <$> D.string

-- Go to 'src/Level06/Conf/File.hs' next
