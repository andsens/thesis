{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Comb.Resolver (
	resolve,
	Resolutions
) where
import qualified Comb.Parser as P

data Value = Value {
	name :: String,
	get_value :: DependencyChain,
	is_false :: DependencyChain
}

data DependencyChain = NoDeps Path

type Path = [P.Content]

data Unresolved = Unresolved { message :: String } deriving (Show)

type Resolutions = ([Value], [Unresolved])

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
inspect res z P.Variable {..}     = variable res z
inspect res z P.Section {..}      = down (section res z) z contents
inspect res z P.XMLTag {..}       = down (down res z attributes) z contents
inspect res z P.EmptyXMLTag {..}  = down res z attributes
inspect res z P.XMLAttribute {..} = down res z contents
inspect res z P.XMLComment {..}   = down res z contents
inspect res z P.Text {..}         = res

variable :: Resolutions -> Zipper -> Resolutions
variable res z = res
section :: Resolutions -> Zipper -> Resolutions
section  res z = res
