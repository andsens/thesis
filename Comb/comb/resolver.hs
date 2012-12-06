{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Comb.Resolver (
	resolve,
	Resolutions,
	Resolution(..)
) where
import qualified Comb.Parser as P
import Text.Parsec.Pos(sourceLine)
import Data.Maybe
import Debug.Trace

type Resolutions = [Resolution]

resolve :: [P.Content] -> Resolutions
resolve (x:xs) = siblings [] (Crumb [] x xs, [])
resolve []     = []


type Zipper = (Crumb, [Crumb])
data Crumb = Crumb {l :: [P.Content], current :: P.Content, r :: [P.Content]}

left :: Zipper -> Zipper
left (Crumb (y:l) x r, trail) = (Crumb l y (x:r), trail)
left (Crumb [] _ _, _) = error "Cannot go left, no more siblings."

right :: Zipper -> Zipper
right (Crumb l x (y:r), trail) = (Crumb (x:l) y r, trail)
right (Crumb _ _ [], _) = error "Cannot go right, no more siblings."

up :: Zipper -> Zipper
up (_, p:trail) = (p, trail)
up (_, []) = error "Cannot go up, no more parents."

siblings :: Resolutions -> Zipper -> Resolutions
siblings res z@(Crumb {r=x:xs,..}, _) = siblings (inspect res z current) (right z)
siblings res z@(Crumb {r=[],..}, _) = inspect res z current

inspect :: Resolutions -> Zipper -> P.Content -> Resolutions
inspect res z P.Variable{}       = resolve_m res z
inspect res z P.Section{..}      = inspect_contents (resolve_m res z) z contents
inspect res z P.XMLTag{..}       = inspect_contents (inspect_contents res z attributes) z contents
inspect res z P.EmptyXMLTag{..}  = inspect_contents res z attributes
inspect res z P.XMLAttribute{..} = inspect_contents res z contents
inspect res z P.XMLComment{..}   = inspect_contents res z contents
inspect res z P.Text{}           = res

inspect_contents :: Resolutions -> Zipper -> [P.Content] -> Resolutions
inspect_contents res (c, trail) (x:xs) = siblings res (Crumb [] x xs, c:trail)
inspect_contents res _ [] = res


data Resolution =
	SectionSelector {
		node :: P.Content,
		path :: Path,
		parent_section :: Maybe Resolution,
		previous_sibling :: Maybe P.Content,
		next_sibling :: Maybe P.Content,
		first_child :: Maybe P.Content,
		last_child :: Maybe P.Content
	} | 
	VariableSelector {
		node :: P.Content,
		path :: Path,
		parent_section :: Maybe Resolution,
		previous_sibling :: Maybe P.Content,
		next_sibling :: Maybe P.Content
	} | Warning {
		node :: P.Content,
		parent_section :: Maybe Resolution,
		message :: String
	} deriving (Show)

find_res :: Resolutions -> P.Content -> Resolution
find_res (r:res) needle
	| (node r) == needle = r
	| otherwise = find_res res needle
find_res [] needle = error ("Resolution for " ++ (show needle) ++ " not found.")

resolve_m :: Resolutions -> Zipper -> Resolutions
resolve_m res z =
	case message of
		Just msg -> (Warning (current $ fst z) parent msg):res
		Nothing -> make_selector res z
	where
		message = resolvable res z
		parent = get_parent res z

make_selector :: Resolutions -> Zipper -> Resolutions
make_selector res z@(Crumb l c@(P.Variable {}) r, _) =
	(VariableSelector c path parent prev next):res
	where
		path   = get_path res z 0
		parent = get_parent res z
		prev   = listToMaybe l
		next   = listToMaybe r
make_selector res z@(Crumb l c@(P.Section {..}) r, _) =
	(SectionSelector c path parent prev next first_c last_c):res
	where
		path    = get_path res z 0
		parent  = get_parent res z
		prev    = listToMaybe l
		next    = listToMaybe r
		first_c = listToMaybe contents
		last_c  = case contents of [] -> Nothing; _ -> Just $ last contents

resolvable :: Resolutions -> Zipper -> Maybe String
resolvable res z@(Crumb (l:ls) _ _, _) = resolvable' res (left z)
resolvable res z@(Crumb [] _ _, p:trail) = resolvable' res (up z)
resolvable res z@(Crumb [] _ _, []) = Nothing

resolvable' :: Resolutions -> Zipper -> Maybe String
resolvable' res z@(Crumb _ v@(P.Variable{escaped=False}) _, _) =
	Just "Path contains unescaped variable"
resolvable' res z@(Crumb _ v@(P.Variable{escaped=True}) _, _) =
	case (find_res res v) of Warning{} -> Just "Unresolved variable found in path"; _ -> resolvable res z
resolvable' res z@(Crumb _ s@(P.Section{}) _, _) =
	case (find_res res s) of Warning{} -> Just "Unresolved section found in path"; _ -> resolvable res z
resolvable' res z = resolvable res z

get_parent :: Resolutions -> Zipper -> Maybe Resolution
get_parent res (_, (Crumb _ s@(P.Section {}) _):trail) = Just (find_res res s)
get_parent res z@(_, _:trail) = get_parent res (up z)
get_parent res (_, []) = Nothing

data Path =
	  Index { index :: Int, parent :: Path }
	| Offset { index :: Int, offset_res :: Resolution }
	| Top { index :: Int } deriving (Show)

get_path :: Resolutions -> Zipper -> Int -> Path
get_path res z@(Crumb {l=l:ls,..}, _) i = get_path' res (left z) i
get_path res z@(Crumb {l=[],..}, p:trail) i = Index i (get_path' res (up z) 0)
get_path res z@(Crumb {l=[],..}, []) i = Top i

get_path' :: Resolutions -> Zipper -> Int -> Path
get_path' res z@(Crumb _ v@(P.Variable{}) _, _) i = Offset i (find_res res v)
get_path' res z@(Crumb _ s@(P.Section{}) _, _) i = Offset i (find_res res s)
get_path' res z@(Crumb _ current _, _) i = get_path res z ((ix_count current) + i)

ix_count :: P.Content -> Int
ix_count P.XMLTag{} = 1
ix_count P.EmptyXMLTag{} = 1
ix_count P.XMLComment{} = 1
ix_count P.XMLAttribute{} = 0
ix_count P.Text{} = 0
ix_count el = error $ "Don't know whether index should be incremented on " ++ (show el) ++ " or not."

