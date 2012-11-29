{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Comb.Resolver (
	resolve,
	Resolutions,
	
) where
import qualified Comb.Parser as P
import Text.Parsec.Pos(sourceLine)
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
inspect res z P.Variable {}       = resolve_mustache res z
inspect res z P.Section {..}      = inspect_contents (resolve_mustache res z) z contents
inspect res z P.XMLTag {..}       = inspect_contents (inspect_contents res z attributes) z contents
inspect res z P.EmptyXMLTag {..}  = inspect_contents res z attributes
inspect res z P.XMLAttribute {..} = inspect_contents res z contents
inspect res z P.XMLComment {..}   = inspect_contents res z contents
inspect res z P.Text {}           = res

inspect_contents :: Resolutions -> Zipper -> [P.Content] -> Resolutions
inspect_contents res (c, trail) (x:xs) = siblings res (Crumb [] x xs, c:trail)
inspect_contents res _ [] = res


data Resolution =
	Selector {
		content :: P.Content,
		path :: Path,
		stack :: [Resolution],
		right_siblings :: [P.Content]
	} | Warning {
		content :: P.Content,
		message :: String,
		stack :: [Resolution]
	}

instance Show Resolution where  
	show Selector {..} = (show (sourceLine $ P.begin content)) ++ " " ++ (P.name content) ++ " | " ++ (show path)
	show Warning {..} = (show (sourceLine $ P.begin content)) ++ " " ++ (P.name content) ++ " " ++ message

data Path = Path { index :: Index, parent :: Path } | End

instance Show Path where  
	show Path {..} = "Path - " ++ (show index) ++ " > parent: " ++ (show parent)
	show End = "End"

data Index = Index { i :: Int, offset :: [Resolution] }

instance Show Index where  
	show Index {..} = "#" ++ (show i) ++ " with " ++ (show (length offset)) ++ " offsets"

resolve_mustache :: Resolutions -> Zipper -> Resolutions
resolve_mustache res z@(Crumb {..}, p:trail) =
	(Selector current (path) stack right_s):res
	where
		stack = get_stack res (up z)
		path = get_path res z
		right_s = (flip filter) r $ \x -> case x of
			P.Variable {escaped=False,..} -> True
			P.Section {contents=c:cs} -> True
			P.XMLTag {} -> True
			P.EmptyXMLTag {} -> True
			P.XMLComment {} -> True
			_ -> False

get_stack :: Resolutions -> Zipper -> [Resolution]
get_stack res z@(Crumb _ s@(P.Section {}) _, p:trail) = (find_res res s):(get_stack res (up z))
get_stack res z@(_, p:trail) = get_stack res (up z)
get_stack res (_, []) = []


find_res :: Resolutions -> P.Content -> Resolution
find_res (r:res) needle
	| (content r) == needle = r
	| otherwise = find_res res needle
find_res [] needle = error ("Resolution for " ++ (show needle) ++ " not found.")

get_path :: Resolutions -> Zipper -> Path
get_path res z@(Crumb {..}, p:trail) = Path (get_index res z) (get_path res (up z))
get_path res z@(Crumb {..}, []) = Path (get_index res z) End

get_index res z@(Crumb {l=x:xs,..}, _) =
	let (i, o) = get_index' res (left z)
	in Index (i+1) o
get_index res (Crumb {l=[],..}, _) = Index 0 []



get_index' res z@(Crumb (l:ls) x@(P.Variable {escaped=False,..}) r, _) =
	(i, (find_res res x):o)
	where (i, o) = get_index' res (left z)
get_index' res z@(Crumb (l:ls) x@(P.Section {contents=c:cs}) r, _) =
	(i, (find_res res x):o)
	where (i, o) = get_index' res (left z)
get_index' res z@(Crumb (l:ls) (P.XMLTag {}) r, _) =
	(i+1, o)
	where (i, o) = get_index' res (left z)
get_index' res z@(Crumb (l:ls) (P.EmptyXMLTag {}) r, _) =
	(i+1, o)
	where (i, o) = get_index' res (left z)
get_index' res z@(Crumb (l:ls) (P.XMLComment {}) r, _) =
	(i+1, o)
	where (i, o) = get_index' res (left z)
get_index' res z@(Crumb (l:ls) x@(P.Variable {escaped=True,..}) r, _) = get_index' res (left z)
get_index' res z@(Crumb (l:ls) x@(P.Section {contents=[]}) r, _) = get_index' res (left z)
get_index' res z@(Crumb (l:ls) (P.Text {}) r, _) = get_index' res (left z)
get_index' res (Crumb [] _ _, _) = (0, [])



