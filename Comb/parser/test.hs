{-# LANGUAGE Arrows #-}

module Main
	where

import Text.XML.HXT.Core
import System.Environment

data FindResult = FindResult {
	resultSetNumber :: String,
	resultNoRecords :: Int,
	resultNoEntries :: Int
} deriving (Eq, Show)

resultParser :: ArrowXml a => a XmlTree FindResult
resultParser =
  deep (isElem >>> hasName "find") >>> proc x -> do
	setNumber <- getText <<< getChildren <<< deep (hasName "set_number") -< x
	noRecords <- getText <<< getChildren <<< deep (hasName "no_records") -< x
	noEntries <- getText <<< getChildren <<< deep (hasName "no_entries") -< x
	returnA -< FindResult setNumber (read noRecords) (read noEntries)


traverse :: ArrowXml a => a XmlTree XmlTree
traverse =
  proc x -> do
		returnA -< x

main :: IO ()
main = do 
  res <- runX $ ( readDocument [withValidate no] "data/moretest.xml" >>> traverse)
  print . head $ res
