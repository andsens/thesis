{-# LANGUAGE Arrows #-}
module Main
	where

import Text.XML.HXT.Core
import Text.XML.HXT.DOM.XmlNode
import Text.XML.HXT.DOM.QualifiedName

type Selector = String
type Placeholder = String
data MappedPlaceholder = Map String Selector deriving Show
data TemplateMap = TM [MappedPlaceholder] deriving Show
--data TreePath = TreePath [XmlNode ]

main :: IO ()
main = do
	res <- runX $ ( readDocument [withValidate no] "data/moretest.xml" >>> getMap)
	print . head $ res

showTree :: ArrowXml a => a XmlTree XmlTree
showTree =
  proc x -> do
		returnA -< x

getMap :: ArrowXml a => a XmlTree TemplateMap
getMap =
  proc x -> do
		returnA -< traverse x


traverse :: XmlTree -> TemplateMap
traverse (NTree (XTag (QualifiedName "/") _) subtree) = traverse subtree
traverse _ = TM [Map "test" "blah"]

--generateSelector :: TreePath -> String
--generateSelector = "test"
