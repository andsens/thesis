{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Main
	where
import Text.XML.Light.Input
import Text.XML.Light.Types
import Debug.Trace

type Placeholder = String
type Selector = String
type Position = [Element]
data Mapping = Mapping Placeholder Selector deriving Show
data TemplateMap = TemplateMap [Mapping] deriving Show

main :: IO ()
main = do
	xml <- readFile "data/profile.hbs"
	let templateMap = traverse [] $ parseXML xml
	print templateMap

traverse :: Position -> [Content] -> [Mapping]
traverse pos (Elem x@(Element name attrs elms _):xs) =
	(traverse (x:pos) elms) ++ (traverse pos xs)
traverse pos (Text (CData kind strdata line):xs) =
	--trace ("Text" ++ strdata)
	([] ++ (traverse pos xs))
traverse pos (CRef x:xs) =
	trace ("Cref" ++ x)
	([] ++ (traverse pos xs))
traverse _ [] = []
--traverse _ _ = [Mapping "blah" "foo"]
