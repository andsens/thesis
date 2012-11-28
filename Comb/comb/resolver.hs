{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Comb.Resolver (
	resolve,
	Resolutions,
	
) where
import qualified Comb.Parser as P
import Text.Parsec.Pos(sourceLine)

type Resolutions = ([Selector], [Warning])

resolve :: [P.Content] -> Resolutions
resolve (x:xs) = siblings ([], []) (Crumb [] x xs, [])
resolve []     = ([], [])


type Zipper = (Crumb, [Crumb])
data Crumb = Crumb {left :: [P.Content], current :: P.Content, right :: [P.Content]}

siblings :: Resolutions -> Zipper -> Resolutions
siblings res z@(Crumb l x (y:r), trail) = siblings (inspect res z x) (Crumb (x:l) y r, trail)
siblings res z@(Crumb {right=[],..}, trail) = inspect res z current

down :: Resolutions -> Zipper -> [P.Content] -> Resolutions
down res (c, trail) (x:xs) = siblings res (Crumb [] x xs, c:trail)
down res _ [] = res

inspect :: Resolutions -> Zipper -> P.Content -> Resolutions
inspect res z P.Variable {..}     = resolve_mustache res z
inspect res z P.Section {..}      = down (resolve_mustache res z) z contents
inspect res z P.XMLTag {..}       = down (down res z attributes) z contents
inspect res z P.EmptyXMLTag {..}  = down res z attributes
inspect res z P.XMLAttribute {..} = down res z contents
inspect res z P.XMLComment {..}   = down res z contents
inspect res z P.Text {..}         = res


data Selector = Selector {
	content :: P.Content,
	path :: Path,
	stack :: [Selector]
} deriving (Show)
data Warning = Warning {
	w_content :: P.Content,
	message :: String,
	w_stack :: [Selector]
} deriving (Show)

data Path = Path { position :: Index, parent :: Path } | End deriving (Show)

data Index = Index { index :: Int, offset :: [Selector] } deriving (Show)

resolve_mustache :: Resolutions -> Zipper -> Resolutions
resolve_mustache res@(selects,warns) z@(Crumb {..}, trail) =
	let path = get_path res z
	in case path of
		Left message ->
			(selects,w:warns)
			where w = Warning current message stack
		Right path ->
			(s:selects,warns)
			where s = Selector current path stack
	where
		stack = get_stack selects z

get_stack :: Resolutions -> Zipper -> Resolutions
get_stack res (Crumb l s@(P.Section {..}) r, p:trail) = (get_resolution res s):(get_stack res (p, trail))
get_stack res (Crumb l s@(P.Section {..}) r, [])      = get_resolution res s
get_stack res (Crumb (x:l) current        r, trail)   = get_stack res (Crumb l x (current:r))
get_stack res (c, []) = []


get_resolution :: Resolutions -> P.Content -> Resolutions
get_resolution (s:selects,warns) needle
	| (content s) == needle = s
	| otherwise = get_resolution (selects,warns) needle
get_resolution ([],w:warns) needle
	| (content w) == needle = w
	| otherwise = get_resolution ([],warns) needle
get_resolution ([], []) needle = error ("Resolution for " ++ (show needle) ++ " not found")

get_path :: Resolutions -> Zipper -> Either String Path
get_path res z = Right Path (Index 0 []) End




