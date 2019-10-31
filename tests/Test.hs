{-# LANGUAGE OverloadedStrings #-}
module Main where

-- | **REMINDER**
-- This level is not an isolated module to complete. This level exists as one
-- starting module: `test/Test.hs`. Which you are to import your most recently
-- completed `Application` to be tested.
--
-- As you progress through the course, you are encouraged to return to this
-- `test/Test.hs` and update it so you're able to be confident that your
-- application will behave as you expect. You may also write your tests before
-- you write your functions, this can be useful when trying to think through a
-- problem.

-- | This is the only location for tests as you progress through the course.

-- | This module starts our very sparse. There are only the imports required to
-- have these initial tests work. Additional tests are for you to build. and may
-- require you to import other modules as you require.
--
-- As you progress through the levels, the initialisation of the 'app' will
-- become more complicated as more components are introduced. Part of the
-- exercise is to work out how to integrate your application with this testing
-- framework.

-- | 'tasty' takes care of managing all of our test cases, running them,
-- checking results and then providing us with a report.
import           Test.Tasty         (defaultMain, testGroup)

-- | 'tasty-wai' makes it easier to create requests to submit to our
-- application, and provides some helper functions for checking our assertions.
import           Test.Tasty.Wai     (assertBody, assertStatus', assertBodyContains,  get, post,
                                     testWai)

-- | For running unit tests for individual functions, we have included the
-- 'tasty-hunit' package. More information is available on the Hackage page:
-- https://hackage.haskell.org/package/tasty-hunit.
--
-- import qualified Test.Tasty.HUnit as HU
--

import           Network.HTTP.Types as HTTP
--import           Network.Wai.Test (assertBodyContains)

-- | This import is provided for you so you can check your work from Level02. As
-- you move forward, come back and import your latest 'Application' so that you
-- can test your work as you progress.
import qualified Level06.Core       as Core
import           Level06.DB
import           Level06.Conf      



main :: IO ()
main = do
  res <- initDB ":memory:" --sqlite in memory
  case (res, makeConfig defaultConf) of
    (Left dbErr, _           ) -> putStrLn $ "error initializing db: " ++ show dbErr
    (_         , Left confErr) -> putStrLn $ "configuration error: "   ++ show confErr 
    (Right dbConn, Right conf) ->
      defaultMain $ testGroup "Applied FP Course - Tests"
      [ testWai (Core.app conf dbConn)"List Topics" $
        get "list" >>= assertStatus' HTTP.status200
        
      , testWai (Core.app conf dbConn) "Empty Input" $ do
          resp <- post "fudge/add" ""
          assertStatus' HTTP.status400 resp
          assertBody "Empty Comment" resp

      , testWai (Core.app conf dbConn) "Add with input" $ do
          resp <- post "fudge/add" "some body content"
          assertStatus' HTTP.status200 resp
          assertBody "Success" resp

      , testWai (Core.app conf dbConn) "Get topic" $
        get "fudge/view" >>= assertStatus' HTTP.status200

      , testWai (Core.app conf dbConn) "Put a comment in puppies topic" $ do
          resp <- post "puppies/add" "puppies are great!"
          assertStatus' HTTP.status200 resp
          assertBody "Success" resp
         
      , testWai (Core.app conf dbConn) "Get comments in puppies topic" $ do
          resp <- get "puppies/view"
          assertStatus' HTTP.status200 resp
          assertBodyContains "puppies are great!" resp

      , testWai (Core.app conf dbConn) "topic puppies exists" $ do
          resp <- get "list"
          assertStatus' HTTP.status200 resp
          assertBodyContains "puppies" resp
      ]
