{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Level08.Types
  ( Error (..)
  , ConfigError (..)
  , PartialConf (..)
  , Port (..)
  , DBFilePath (..)
  , Conf (..)
  , FirstAppDB (..)
  , RqType (..)
  , ContentType (..)
  , Comment (..)
  , Topic
  , CommentText
  , partialConfDecoder
  , mkTopic
  , getTopic
  , mkCommentText
  , getCommentText
  , renderContentType
  , fromDBComment
  , confPortToWai
  , encodeComment
  , encodeTopic
  , getDBFilePath
  , port
  , dbFilePath
  ) where

import           System.IO.Error                    (IOError)

import           GHC.Word                           (Word16)

import           Data.ByteString                    (ByteString)
import           Data.Text                          (pack)

import           Data.Functor.Contravariant         ((>$<))
import           Data.Semigroup                     (Last (Last), Semigroup ((<>)))

import           Data.Time                          (UTCTime)
import qualified Data.Time.Format                   as TF

import           Waargonaut.Decode                  (CursorHistory, Decoder)
import qualified Waargonaut.Decode                  as D
import           Waargonaut.Decode.Error            (DecodeError)

import           Waargonaut.Encode                  (Encoder)
import qualified Waargonaut.Encode                  as E

import           Database.SQLite.Simple             (Connection)
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level08.DB.Types                   (DBComment (..), dbCommentComment, dbCommentId, dbCommentTime, dbCommentTopic)

import           Level08.Types.Error                (Error (DBError, EmptyCommentText, EmptyTopic, UnknownRoute))

import           Level08.Types.CommentText          (CommentText,
                                                     encodeCommentText,
                                                     getCommentText,
                                                     mkCommentText)

import           Level08.Types.Topic                (Topic, encodeTopic,
                                                     getTopic, mkTopic)

import           Control.Lens

newtype CommentId = CommentId Int
  deriving (Show)

encodeCommentId :: Applicative f => Encoder f CommentId
encodeCommentId = (\(CommentId i) -> i) >$< E.int

data Comment = Comment
  { _commentId    :: CommentId
  , _commentTopic :: Topic
  , _commentText  :: CommentText
  , _commentTime  :: UTCTime
  }
  deriving Show

makeLenses ''CommentId
makeLenses ''Comment
makeLenses ''Topic

encodeISO8601DateTime :: Applicative f => Encoder f UTCTime
encodeISO8601DateTime = pack . TF.formatTime tl fmt >$< E.text
  where
    fmt = TF.iso8601DateFormat (Just "%H:%M:%S")
    tl = TF.defaultTimeLocale { TF.knownTimeZones = [] }

encodeComment :: Applicative f => Encoder f Comment
encodeComment = E.mapLikeObj $ \c ->
  E.atKey' "id"    encodeCommentId       (c ^. commentId) .
  E.atKey' "topic" encodeTopic           (c ^. commentTopic) .
  E.atKey' "text"  encodeCommentText     (c ^. commentText) .
  E.atKey' "time"  encodeISO8601DateTime (c ^. commentTime)

-- For safety we take our stored DBComment and try to construct a Comment that
-- we would be okay with showing someone. However unlikely it may be, this is a
-- nice method for separating out the back and front end of a web app and
-- providing greater guarantees about data cleanliness.
fromDBComment
  :: DBComment
  -> Either Error Comment
fromDBComment dbc =
  Comment (CommentId     $ dbc ^. dbCommentId)
      <$> (mkTopic       $ dbc ^. dbCommentTopic)
      <*> (mkCommentText $ dbc ^. dbCommentComment)
      <*> (pure          $ dbc ^. dbCommentTime)

data RqType
  = AddRq Topic CommentText
  | ViewRq Topic
  | ListRq

-- Provide a type to list our response content types so we don't try to
-- do the wrong thing with what we meant to be used as text or JSON etc.
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

-- This is an alternative way of defining a `newtype`. You define it as a simple
-- record and this lets you specify an unwrapping function at the same time. Which
-- technique you choose is a matter for your specific needs and preference.
--
newtype Port = Port
  { _getPort :: Word16 }
  deriving (Eq, Show)

newtype DBFilePath = DBFilePath
  { _getDBFilePath :: FilePath }
  deriving (Eq, Show)

-- The ``Conf`` type will need:
-- - A customisable port number: ``Port``
-- - A filepath for our SQLite database: ``DBFilePath``
data Conf = Conf
  { _port       :: Port
  , _dbFilePath :: DBFilePath
  }
  deriving Eq

makeLenses ''Port
makeLenses ''DBFilePath
makeLenses ''Conf

-- We're storing our Port as a Word16 to be more precise and prevent invalid
-- values from being used in our application. However Wai is not so stringent.
-- To accommodate this and make our lives a bit easier, we will write this
-- helper function to take ``Conf`` value and convert it to an ``Int``.
confPortToWai
  :: Conf
  -> Int
confPortToWai conf = fromIntegral (conf ^. port . getPort)
  --fromIntegral . getPort . port

-- Similar to when we were considering our application types, leave this empty
-- for now and add to it as you go.
data ConfigError
  = BadConfFile (DecodeError, CursorHistory)
  | MissingPort
  | MissingDBFilePath
  | JSONDecodeError String
  | ConfigFileReadError IOError
  deriving Show

-- Our application will be able to load configuration from both a file and command line
-- input. We want to be able to use the command line to temporarily override the
-- configuration from our file. How do we combine the different inputs to enable this
-- property?

-- We want the command line configuration to take precedence over the File configuration,
-- so if we think about combining each of our ``Conf`` records, we want to be able to
-- write something like this:

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

-- We need to define a ``Semigroup`` instance for ``PartialConf``. We define our ``(<>)``
-- function to lean on the ``Semigroup`` instance for Last to always get the last value.
instance Semigroup PartialConf where
  a <> b = PartialConf
    { pcPort       = pcPort a <> pcPort b
    , pcDBFilePath = pcDBFilePath a <> pcDBFilePath b
    }

-- When it comes to reading the configuration options from the command-line, we
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
  <$> lastAt "port" D.integral Port
  <*> lastAt "dbFilePath" D.string DBFilePath
  where
    lastAt k d c = fmap (Last . c) <$> D.atKeyOptional k d

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
newtype FirstAppDB = FirstAppDB
  { dbConn  :: Connection
  }
