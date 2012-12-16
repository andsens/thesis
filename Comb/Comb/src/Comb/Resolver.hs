{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Comb.Resolver (
	resolve,
	Resolutions,
	Resolution(..),
	Zipper(..),
	Path(..),
	fq_name
) where
import qualified Comb.Parser as P
import Data.Maybe(listToMaybe)

type Resolutions = [Resolution]

resolve :: [P.Content] -> Resolutions
resolve (x:xs) = reverse (siblings [] (Crumb [] x xs, []))
resolve []     = []

type Zipper = (Crumb, [Crumb])
data Crumb = Crumb {l :: [P.Content], current :: P.Content, r :: [P.Content]}

siblings :: Resolutions -> Zipper -> Resolutions
siblings res z@(Crumb l x (y:r), trail) = siblings (inspect res z x) (Crumb (x:l) y r, trail)
siblings res z@(Crumb _ x [], _) = inspect res z x

inspect :: Resolutions -> Zipper -> P.Content -> Resolutions
inspect res z P.Variable{}       = make_selector res z
inspect res z P.Section{..}      = inspect_contents (make_selector res z) z contents
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
		section :: Maybe Resolution,
		prev :: Maybe P.Content,
		next :: Maybe P.Content,
		first_c :: Maybe P.Content,
		last_c :: Maybe P.Content
	} | VariableSelector {
		node :: P.Content,
		path :: Path,
		section :: Maybe Resolution,
		prev :: Maybe P.Content,
		next :: Maybe P.Content
	}


instance Show Resolution where
	show s@SectionSelector{node=P.Section{inverted=False,..},..} = '#':fq_name s
	show s@SectionSelector{node=P.Section{inverted=True,..},..} = '^':fq_name s
	show v@VariableSelector{node=P.Variable{escaped=True,..},..} = fq_name v
	show v@VariableSelector{node=P.Variable{escaped=False,..},..} = '{':fq_name v

fq_name :: Resolution -> String
fq_name res = (scope $ section res) ++ (P.name (node res))

scope :: Maybe Resolution -> String
scope (Just SectionSelector{..}) = (scope section) ++ (P.name node) ++ ">"
scope Nothing = ""

find_res :: Resolutions -> P.Content -> Resolution
find_res (r:res) needle
	| (node r) == needle = r
	| otherwise = find_res res needle
find_res [] needle = error ("Resolution for " ++ (show needle) ++ " not found.")

make_selector :: Resolutions -> Zipper -> Resolutions
make_selector res z@(Crumb l c@(P.Section {..}) r, _) =
	(SectionSelector c path section prev next first_c last_c):res
	where
		path    = get_path res z
		section = get_section res z
		prev    = listToMaybe l
		next    = listToMaybe r
		first_c = listToMaybe contents
		last_c  = case contents of [] -> Nothing; _ -> Just $ last contents
make_selector res z@(Crumb l c@(P.Variable {}) r, _) =
	(VariableSelector c path section prev next):res
	where
		path    = get_path res z
		section = get_section res z
		prev    = listToMaybe l
		next    = listToMaybe r

get_section :: Resolutions -> Zipper -> Maybe Resolution
get_section res (_, (Crumb _ s@(P.Section {}) _):trail) = Just (find_res res s)
get_section res (_, p:trail) = get_section res (p, trail)
get_section res (_, []) = Nothing

data Path =
	  Attribute { name :: String, parent :: Path }
	| Index { index :: Int, parent :: Path }
	| Offset { offset :: Resolution }
	| Child { offset :: Resolution }
	| Root
	deriving (Show)

get_path :: Resolutions -> Zipper -> Path
-- Don't include previous textnodes, they will be merged with the section or variable
get_path res (Crumb (y@P.Text{}:l) x r, trail) = backtrack res (Crumb l y (x:r), trail) 0
get_path res (Crumb (y:l) x r, trail) = backtrack res (Crumb l y (x:r), trail) 1
get_path res (Crumb [] _ _, p:trail) = Index 0 (backtrack_parent res (p, trail))
get_path res (Crumb [] _ _, []) = Index 0 Root

backtrack :: Resolutions -> Zipper -> Int -> Path
-- Offsets are special in terms of indexes, because we do not count the offset node itself
backtrack res (Crumb _ s@(P.Section{}) _, _) i = Index (i-1) (Offset (find_res res s))
backtrack res (Crumb _ v@(P.Variable{}) _, _) i = Index (i-1) (Offset (find_res res v))
backtrack res (Crumb (y:l) x r, trail) i = backtrack res (Crumb l y (x:r), trail) (i+1)
backtrack res (Crumb [] x r, p:trail) i = Index i (backtrack_parent res (p, trail))
backtrack res (Crumb [] x r, []) i = Index i Root

backtrack_parent res (Crumb _ s@(P.Section{}) _, _) = Child (find_res res s)
backtrack_parent res (Crumb (y:l) x r, trail) = backtrack res (Crumb l y (x:r), trail) 1
backtrack_parent res (Crumb _ P.XMLAttribute{..} _, p:trail) = Attribute name (backtrack_parent res (p, trail))
backtrack_parent res (Crumb [] _ _, p:trail) = Index 0 (backtrack_parent res (p, trail))
backtrack_parent res (Crumb [] _ _, []) = Index 0 Root















