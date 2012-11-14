{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Main
	where
import Text.XML.Light.Input
import Text.XML.Light.Types
import Text.Regex.Posix
--import Text.Regex.Base
--import Debug.Trace
--import Data.Maybe(isJust)

data Location = Loc Int Element deriving Show
type Trail = [Location]

data Substring = Substr { before :: Int, after :: Int}
data Attribute = Attr { name :: String, substring :: Substring }
data Position  = Attribute | Substring | Fullstring deriving Show

data Map = Map { placeholder :: String, trail :: Trail, position :: Position } deriving Show
type Function = String
data CSSMap = CSS { locator :: String, path :: String, subselector :: Function } deriving Show

main :: IO ()
main = do
	xml <- readFile "data/profile.hbs"
	let templateMap = traverse [] 0 (parseXML xml)
	print $ map toCSS templateMap

toCSS :: Map -> CSSMap
toCSS Map{..} = CSS placeholder (foldr prependPath "" trail) ""

prependPath :: Location -> String -> String
prependPath (Loc i (Element (QName {..}) _ _ _)) s
	| length s > 0 = s++">"++qName++":nth-child(" ++ show (i+1) ++ ")"
	| otherwise    = qName++":nth-child(" ++ show (i+1) ++ ")"

traverse :: Trail -> Int -> [Content] -> [Map]
traverse trail index (x@(Elem el@Element{..}):xs) =
	let
		current  = inspect ((Loc index el):trail) index x
		children = traverse ((Loc index el):trail) index elContent
		siblings = traverse trail (index+1) xs
	in prependMaybe current children ++ siblings
traverse trail index (x:xs) =
	let
		current  = inspect trail index x
		siblings = traverse trail (index+1) xs
	in prependMaybe current siblings
traverse trail index [] = []

prependMaybe :: Maybe a -> [a] -> [a]
prependMaybe (Just x) xs = x:xs
prependMaybe Nothing xs  = xs

inspect :: Trail -> Int -> Content -> Maybe Map
inspect trail index (Elem el) = -- Check attributes
	Nothing
inspect trail index (Text CData{..}) -- Check substrings
	| length match > 0 = Just (Map match trail Fullstring)
	| otherwise = Nothing
	where
		match = cdData =~ "{{[^}]+}}"
inspect trail index (CRef string) = -- Do nothing
	Nothing

