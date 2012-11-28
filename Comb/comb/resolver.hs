{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Comb.Resolver (
	results,
	Resolutions
) where
import qualified Comb.Parser as P

data Value = Value {
	name :: String,
	get_value :: DependencyChain
	is_false :: DependencyChain
}

data DependencyChain = Chain | Path

type Resolutions = ([Value], [Unresolved])

resolve :: [P.Content] -> Resolutions
resolve x:xs = siblings ( ([], []) (Crumb [] x xs, []) )
resolve []   = ([], [])

type Zipper = (Crumb, [Crumb])
data Crumb = Crumb {left :: [P.Content], current :: P.Content, right :: [P.Content]}

siblings res z@(Crumb l x y:r, trail) = siblings (inspect x res z) (Crumb (x:l) y r, trail)
siblings res z@(Crumb {right=[],..}, trail) = inspect current res z

down res field (c@Crumb l x r, trail)
	| length subtree == 0 = res
	| otherwise           = siblings res (Crumb [] (head subtree) (tail subtree), c:trail)
	where subtree = field x

inspect Section     x res z = down (section res z) P.contents z
inspect XMLTag      x res z = down (down res P.attributes z) P.contents z
inspect Variable    x res z = variable res z
inspect EmptyXMLTag x res z = down res P.attributes z
inspect XMLComment  x res z = down res P.contents z
inspect Text        x res z = res

