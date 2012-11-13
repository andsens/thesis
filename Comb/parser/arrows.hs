{-# LANGUAGE Arrows #-}
module Main where

import qualified Data.Map as M
import Text.XML.HXT.Core

classes :: (ArrowXml a) => a XmlTree (M.Map String String)
classes = listA (divs >>> pairs) >>> arr M.fromList
  where
    divs = getChildren >>> hasName "div"
    pairs = proc div -> do
      cls <- getAttrValue "class" -< div
      val <- deep getText         -< div
      returnA -< (cls, val)

getValues :: (ArrowXml a) => [String] -> a XmlTree (String, Maybe String)
getValues cs = classes >>> arr (zip cs . lookupValues cs) >>> unlistA
  where lookupValues cs m = map (flip M.lookup m) cs

xml = "<div><div class='c1'>a</div><div class='c2'>b</div>\
      \<div class='c3'>123</div><div class='c4'>234</div></div>"

values :: [(String, Maybe String)]
values = runLA (xread >>> getValues ["c1", "c2", "c3", "c4"]) xml

main = print values
