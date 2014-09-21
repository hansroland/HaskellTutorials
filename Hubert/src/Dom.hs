-- -------------------------------------------------------------------
-- Hubert - a simple browser in Haskell
-- -------------------------------------------------------------------
--
-- See:
--  http://hrothen.github.io/2014/09/05/lets-build-a-browser-engine-in-haskell/
--
-- -------------------------------------------------------------------

module Dom
    where

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM


-- | A browsers DOM is a tree of Nodes representing the
-- structure of an HTML page
data NTree a = NTree a [NTree a]
    deriving (Show, Eq)

-- | We will have only element types
data NodeType = Text T.Text
              | Element ElementData
    deriving (Show, Eq)

-- | An alias for a node
type Node = NTree NodeType

-- | We map from text strings to text strings
type AttrMap = HM.HashMap T.Text T.Text


data ElementData = ElementData T.Text AttrMap
    deriving (Show, Eq)

-- | Constructor function for Node Type
text :: T.Text -> Node
text = flip NTree [] . Text

elem :: T.Text -> AttrMap -> [Node] -> Node
elem name atts cs = NTree (Element (ElementData name atts)) cs

