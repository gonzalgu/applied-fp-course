{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Level06.Conf
    ( parseOptions
    , defaultConf
    , makeConfig
    ) where

import           GHC.Word                 (Word16)

import           Data.Bifunctor           (first)
--import           Data.Monoid              (Last, (<>))
import           Data.Semigroup           (Last(..), Semigroup ((<>)))

import           Level06.AppM             (AppM(..),runAppM, liftEither, liftIO)
import           Level06.Types            (PartialConf(..), Conf(..), ConfigError(..),
                                           DBFilePath (DBFilePath),
                                           Port (..))

import           Level06.Conf.CommandLine (commandLineParser)
import           Level06.Conf.File        (parseJSONConfigFile)

-- | For the purposes of this application we will encode some default values to
-- ensure that our application continues to function in the event of missing
-- configuration values from either the file or command line inputs.
defaultConf
  :: PartialConf
defaultConf = PartialConf (Just . Last . Port $ 3000) (Just . Last . DBFilePath $ "app_db.db")


-- | We need something that will take our PartialConf and see if can finally build
-- a complete ``Conf`` record. Also we need to highlight any missing values by
-- providing the relevant error.
makeConfig
  :: PartialConf
  -> Either ConfigError Conf
makeConfig pc =
  case (pcPort pc, pcDBFilePath pc) of
    (Nothing,_) -> Left . UndefinedProperty $ "pcPort"
    (_,Nothing) -> Left . UndefinedProperty $ "pcDBFilePath"
    (Just(Last p), Just(Last path)) -> Right $ Conf p path
    
  

-- | This is the function we'll actually export for building our configuration.
-- Since it wraps all our efforts to read information from the command line, and
-- the file, before combining it all and returning the required information.
--
-- Remember that we want the command line configuration to take precedence over
-- the File configuration, so if we think about combining each of our ``Conf``
-- records. By now we should be able to write something like this:
--
-- ``defaults <> file <> commandLine``
--
parseOptions
  :: FilePath
  -> AppM ConfigError Conf
parseOptions fp =  do
  fileConf <- parseJSONConfigFile fp
  commandConf <- liftIO commandLineParser
  liftEither $ makeConfig $  defaultConf <> fileConf <> commandConf
  
  --parseJSONConfigFile "files/appconfig.json"
  -- Parse the options from the config file: "files/appconfig.json"
  -- Parse the options from the commandline using 'commandLineParser'
  -- Combine these with the default configuration 'defaultConf'
  -- Return the final configuration value
--  error "parseOptions not implemented"
