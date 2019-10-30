{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}
module Level06.Conf.File where

import           Data.ByteString            (ByteString,readFile)
import           Prelude            hiding  (readFile)
--import           Data.Text.Encoding         (encodeUtf8)

import           Data.Text                  (Text,pack)

import           Data.Bifunctor             (bimap, first)
import           Data.Monoid                (Last (Last))

import           Control.Exception          (SomeException, Exception, try, displayException)

import qualified Data.Attoparsec.ByteString as AB

import           Waargonaut                 (Json)
import qualified Waargonaut.Decode          as D
import           Waargonaut.Decode.Error    (DecodeError (ParseFailed))

import           Level06.AppM               
import           Level06.Types              (ConfigError (..), partialConfDecoder ,
                                             PartialConf (PartialConf))
-- $setup
-- >>> :set -XOverloadedStrings

-- | The configuration file is in the JSON format, so we need to write a
-- 'waargonaut' 'Decoder' to go from JSON to our 'PartialConf'.
--
-- Update these tests when you've completed this function.
--
-- >>> runAppM $ readConfFile "badFileName.no"
-- Left (<YourErrorConstructorHere> "badFileName.no: openBinaryFile: does not exist (No such file or directory)")
-- >>> runAppM $ readConfFile "files/test.json"
-- Right "{\n  \"foo\": 33\n}\n"
--
readConfFile
  :: FilePath
  -> AppM ConfigError ByteString
readConfFile fp = AppM $ do{
  result <- try (readFile fp) :: IO(Either SomeException ByteString);
  return $ bimap (FileNotFound . pack . displayException) id result
  }
    
  -- Reading a file may throw an exception for any number of
  -- reasons. Use the 'try' function from 'Control.Exception' to catch
  -- the exception and turn it into an error value that is thrown as
  -- part of our 'AppM' transformer.
  --
  -- No exceptions from reading the file should escape this function.
  --
  --error "readConfFile not implemented"

-- | Construct the function that will take a ``FilePath``, read it in, decode it,
-- and construct our ``PartialConf``.
parseJSONConfigFile
  :: FilePath
  -> AppM ConfigError PartialConf
parseJSONConfigFile fp = do
  conf <- readConfFile fp
  let e = D.pureDecodeFromByteString AB.parseOnly partialConfDecoder conf
  liftEither $ bimap (BadConfFile . fst) id e
  {-
  case e of
    Left (dec,_)      -> liftEither $ Left (BadConfFile dec)
    Right partialConf -> liftEither $ Right partialConf
    -}

  
  

-- Go to 'src/Level06/Conf.hs' next.
