{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}

-- ------------------------------------------------------------------------------------
-- CSS.hs
-- ------------------------------------------------------------------------------------
--
-- See: http://hrothen.github.io/2014/09/19/lets-build-a-browser-engine-in-haskell-part-3/
--
-- ------------------------------------------------------------------------------------

module CSS (

        ) here

import Prelude hiding (id)

import Data.Word (Word(..), Word8(..))
import Data.List(sortBy)
import Data.Maybe (maybe)
import Numeric (readFloat, readHex)
import Control.Applicative( (<*), (*>), (<$>), (<*>))

import Text.Parsec
import Text.Parsec.Text

import qualified Data.Text as T

-- data types to represent our CSS

data Stylesheet = Stylesheet [Rule]
   deriving (Show, Eq)

data Rule [Selector] [Declaration]
   deriving (Show, Eq)


data Selector = Simple (Maybe T.Text) (Maybe T.Text) [T.Text]
    deriving (Show, Eq)
    -- only handle simple selectors for now

data Declaration = Dclaration T.Text Value
    deriving (Show, Eq)

data Value = Keyword T.Text
           | Color Word8 Word8 Word8 Word8
           | Length Float Unit
    deriving (Show, Eq)

data Unit = Px   -- only Px for now
    deriving (Show, Eq)

-- | an empty selector
nilS = Simple Nothing Nothing []