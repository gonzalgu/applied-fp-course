{-# LANGUAGE TemplateHaskell #-}
module Level08.DB.Types where

import           Data.Text                      (Text)
import           Data.Time                      (UTCTime)
import           Control.Lens
import           Database.SQLite.Simple.FromRow (FromRow (fromRow), field)

-- To try to avoid leaking various types and expected functionality around the
-- application, we create a stand alone type that will represent the data we
-- store in the database. In this instance, it is the raw types that make up a
-- comment.
data DBComment = DBComment
  { _dbCommentId      :: Int
  , _dbCommentTopic   :: Text
  , _dbCommentComment :: Text
  , _dbCommentTime    :: UTCTime
  }
  deriving Show

makeLenses ''DBComment  
  
  

-- This type class instance comes from our DB package and tells the DB package
-- how to decode a single row from the database into a single representation of
-- our type. This technique of translating a result row to a type will differ
-- between different packages/databases.
instance FromRow DBComment where
  fromRow = DBComment
            -- field :: FromField a => RowParser a
            <$> field
            <*> field
            <*> field
            <*> field
