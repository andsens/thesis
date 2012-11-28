{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Comb.Resolver (
	resolve,
	Resolutions
) where
import qualified Comb.Parser as P

--data Value = Value {
--	name :: String,
--	get_value :: DependencyChain
--	is_false :: DependencyChain
--}

--data DependencyChain = Chain | Path

data Value = Value { name :: String } deriving (Show)
data Unresolved = Unresolved { message :: String } deriving (Show)

type Resolutions = ([Value], [Unresolved])

resolve :: [P.Content] -> Resolutions
resolve (x:xs) = siblings ([Value "blah"], []) (Crumb [] x xs, [])
resolve []     = ([], [])

type Zipper = (Crumb, [Crumb])
data Crumb = Crumb {left :: [P.Content], current :: P.Content, right :: [P.Content]}

siblings res z@(Crumb l x (y:r), trail) = siblings (inspect x res z) (Crumb (x:l) y r, trail)
siblings res z@(Crumb {right=[],..}, trail) = inspect current res z

down res (c, trail) (x:xs) = siblings res (Crumb [] x xs, c:trail)
down res _ [] = res

inspect P.Variable {..}     res z = variable res z
inspect P.Section {..}      res z = down (section res z) z contents
inspect P.XMLTag {..}       res z = down (down res z attributes) z contents
inspect P.EmptyXMLTag {..}  res z = down res z attributes
inspect P.XMLAttribute {..} res z = down res z contents
inspect P.XMLComment {..}   res z = down res z contents
inspect P.Text {..}         res z = res

variable res z = res
section  res z = res
