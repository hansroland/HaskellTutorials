{-# LANGUAGE OverloadedStrings #-}

module Main where


import Prelude hiding (elem)
import Data.Either (either)

import Test.HUnit

import Text.Parsec hiding (parseTest)
import Text.Parsec.Text

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

import HTML.Parsec as PS
import Dom

-- simple tests
testText = text "candygram"
testElem = elem "p" (HM.singleton "ham" "doctor") [text "sup"]

-- a small test html page
-- note: multi-line strings in Haskell
html = "<html>\n\
       \      <head>\n\
       \         <title>Test</title>\n\
       \      </head>\n\
       \      <p class=\"inner\">\n\
       \         Hello, <span id=\"name\">world!</span>\n\
       \      </p>\n\
       \      <p class=\"inner\">\n\
       \          Goodbye!\n\
       \      </p>\n\
       \</html>"

-- | the expected result of parsing the test page
dom = elem "html" HM.empty [head, p1, p2]
  where
    head  = elem "head"  HM.empty [title]
    title = elem "title" HM.empty [text "Test"]
    p1    = elem "p"    (HM.singleton "class" "inner") [hello, span]
    hello = text "Hello, "
    span  = elem "span" (HM.singleton "id" "name") [text "world!"]
    p2    = elem "p"    (HM.singleton "class" "inner") [text "Goodbye!\n      "]


-- | Helper function to test on succeed AND correct value
parseTest msg e = TestCase . either (assertFailure . show) (assertEqual msg e)

-- -------------------- PARSEC TESTS ---------------------------
htmlPS = parseTest "for valid html" dom $ PS.parseHtml html

textPS = parseTest "for valid text" testText $ 
         parse PS.parseText "" $ T.pack "candygram"

elemPS = parseTest "for valid elem" testElem $
         parse PS.parseElement "" $ T.pack "<p ham=\"doctor\">sup</p>"

-- ------------------- RUN TESTS -------------------------------
main = runTestTT tests

tests = TestList [TestLabel "Parsec html" htmlPS,
                  TestLabel "Parsec text" textPS,
                  TestLabel "Parsec elem" elemPS]


