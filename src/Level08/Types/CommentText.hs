{-# LANGUAGE TemplateHaskell #-}
module Level08.Types.CommentText
  ( CommentText
  , mkCommentText
  , getCommentText
  , encodeCommentText
  ) where

import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E

import           Data.Functor.Contravariant ((>$<))
import           Data.Text                  (Text)

import           Level08.Types.Error        (Error (EmptyCommentText),
                                             nonEmptyText)
import Control.Lens                                             

newtype CommentText = CommentText Text
  deriving (Show)
  
makeLenses ''CommentText  

encodeCommentText :: Applicative f => Encoder f CommentText
encodeCommentText = getCommentText >$< E.text

mkCommentText
  :: Text
  -> Either Error CommentText
mkCommentText =
  nonEmptyText CommentText EmptyCommentText

getCommentText
  :: CommentText
  -> Text
getCommentText (CommentText t) =
  t
