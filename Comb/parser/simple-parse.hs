{-# LANGUAGE NamedFieldPuns, RecordWildCards, DeriveDataTypeable #-}
module Main
	where
import Text.XML.Light.Input
import Text.XML.Light.Types
import Text.Regex.PCRE
import Text.JSON(showJSObject)
import Text.JSON.Pretty(pp_js_object)
import Text.JSON.Types(JSObject(JSONObject), JSValue(..), toJSString, set_field, get_field)
import Data.Maybe(maybeToList)
import Debug.Trace
import Data.Maybe(isJust)


main :: IO ()
main = do
	xml <- readFile "data/profile.hbs"
	let templateMap = traverse [] 0 (parseXML xml)
	--mapM_ print . show $ map createSelector templateMap
	let json = toJSON (JSONObject []) (map createSelector templateMap)
	putStrLn $ show (pp_js_object json)
	--putStrLn ""

toJSON :: JSObject JSValue -> [Selector] -> JSObject JSValue
toJSON object (Selector {..}:xs) =
	set_field newObject name $ JSArray ( (jQuery path subselector) : oldArray )
	where
		newObject = toJSON object xs
		oldProp   = get_field newObject name
		oldArray  = (case oldProp of { Just (JSArray x) -> x; _ -> []})
toJSON object [] = object

jQuery :: CSSPath -> Function -> JSValue
jQuery path fn = JSString $ toJSString $ "function($el) {return $el.find('" ++ path ++ "')." ++ fn ++ "}"


type CSSPath  = String
type Function = String
data Selector = Selector { name :: Name, path :: CSSPath, subselector :: Function } deriving Show

createSelector :: Map -> Selector
createSelector Map{..} = Selector m_name (foldr prependPath "" trail) (createFn position)

createFn :: Position -> Function
createFn (Data Full) = "text()"
createFn (Data (Substr {..})) = "substring("++(show before)++", -"++(show after)++")"

prependPath :: Location -> String -> String
prependPath (Loc i (Element (QName {..}) _ _ _)) s
	| length s > 0 = s++">"++qName++":nth-child(" ++ show (i+1) ++ ")"
	| otherwise    = qName++":nth-child(" ++ show (i+1) ++ ")"


data Location = Loc Int Element deriving Show
type Trail = [Location]

traverse :: Trail -> Int -> [Content] -> [Map]
traverse trail index (x@(Elem el@Element{..}):xs) =
	let
		current  = inspect ((Loc index el):trail) index x
		children = traverse ((Loc index el):trail) 0 elContent
		siblings = traverse trail (index+1) xs
	in current ++ children ++ siblings
traverse trail index (x:xs) =
	let
		current  = inspect trail index x
		siblings = traverse trail index xs
	in current ++ siblings
traverse trail index [] = []


type Name = String
data StrPos   = Substr { before :: Int, after :: Int} | Full deriving Show
data Position = Attr { attrName :: String, substring :: StrPos } | Data StrPos deriving Show
data Map = Map { m_name :: Name, trail :: Trail, position :: Position } deriving Show

inspect :: Trail -> Int -> Content -> [Map]
inspect trail index (Elem el) = -- Check attributes
	[]
inspect trail index (Text CData{..}) = -- Check substrings
	case substring of
		Just substring -> [Map (fst substring) trail (Data $ snd substring)]
		_ -> []
	where
		substring = findMustache cdData
inspect trail index (CRef string) = -- Do nothing
	[]

findMustache :: String -> Maybe (Name, StrPos)
findMustache string = findOpenMustache 0 string

findOpenMustache :: Int -> String -> Maybe (Name, StrPos)
findOpenMustache before ('{':'{':xs) =
	Just (name, position)
	where
		closing  = findCloseMustache "" xs
		name     = fst closing
		after    = snd closing
		position = if (before == 0 && after == 0) then Full else Substr before after
findOpenMustache before (x:xs) = findOpenMustache (before+1) xs
findOpenMustache _ "" = Nothing

findCloseMustache :: Name -> String -> (Name, Int)
findCloseMustache name ('}':'}':xs) = (reverse name, length xs)
findCloseMustache name (x:xs) = findCloseMustache (x:name) xs
findCloseMustache name [] = error "End of string encountered"



