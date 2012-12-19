{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Comb.Filter (
	filter_resolutions
) where
import qualified Comb.Parser as P
import qualified Comb.Resolver as R
import Text.Parsec.Pos(sourceLine,sourceName)
import Data.List(sort)
import Debug.Trace

type Errors = [Error]

data Error   =
	Warning {
  	message :: String,
  	resolution :: R.Resolution
	} | Error {
  	message :: String,
  	resolution :: R.Resolution
  } | Unresolved {
  	resolution :: R.Resolution,
  	cause :: R.Resolution
  } deriving (Eq)

--txtdef="\e[0m"    # revert to default color
--bldred="\e[1;31m" # bold red
--txtblu="\e[0;34m" # blue
--txtbld="\e[1m"    # bold
--txtund="\e[4m"    # underline
instance Show Error where
	show (Warning message res) = "\ESC[0;34mWarning\ESC[0m: " ++ (location res) ++ "\n    " ++ message
	show (Error message res) = "\ESC[1;31mError\ESC[0m: " ++ (location res) ++ "\n    " ++ message
	show (Unresolved res cause) = "\ESC[0;37mUnresolved\ESC[0m " ++ (short_location res) ++ " caused by " ++ (short_location cause)

instance Ord Error where
	compare x y = compare (resolution x) (resolution y)

location res =
	name ++ " '" ++ (P.name node) ++ "' in " ++ filename ++ ":" ++ line
	where
		node = R.node res
		filename = (sourceName . P.begin) node
		line = show $ (sourceLine . P.begin) node
		name = typeof res
short_location res =
	name ++ " '" ++ (P.name node) ++ "' on line " ++ line
	where
		node = R.node res
		line = show $ (sourceLine . P.begin) node
		name = typeof res

typeof R.SectionSelector{..}  = if (P.inverted node) then "inverted section" else "section"
typeof R.PartialSelector{}    = "partial"
typeof R.VariableSelector{..} = if (P.escaped node) then "escaped variable" else "unescaped variable"

filter_resolutions :: R.Resolutions -> (R.Resolutions, Errors)
filter_resolutions resolutions =
	let errors = run_checks resolutions [] checks
	in (filter (not . has_error errors) resolutions, sort errors)

run_checks resolutions errs (check:checks) = run_checks resolutions (check resolutions errs) checks
run_checks resolutions errs [] = errs

checks = [
	  run_check unescaped_offset
	, run_check empty_section
	, run_check unescaped_pos
	, run_check partial_only_child
	--, check if two variables in the same text section
	, run_check no_lookahead
	, run_check ambiguous_boundaries
	--, sections_with_errors []
	, path_with_errors []
	]

run_check check (r:rs) errs = run_check check rs (check r errs)
run_check check [] errs = errs

-- Unescaped offsets
unescaped_offset resolution errs =
	case get_path_top (R.path resolution) of
		R.Offset (root@R.VariableSelector{node=P.Variable{escaped=False}}) ->
			(Unresolved resolution root):errs
		_ -> errs

-- Warns if there were found empty sections
empty_section r@R.SectionSelector{node=P.Section{contents=[]}} errs =
	(Warning "The section is empty" r):errs
empty_section _ errs = errs

-- Unescaped variable is always the last child of a proper XMLTag, Comment or Root
unescaped_pos r@R.VariableSelector{node=P.Variable{escaped=False},next=(Just something)} errs =
	(Error "An unescaped variable must be the last child of a node" r):errs
unescaped_pos r@R.VariableSelector{node=P.Variable{escaped=False},..} errs
	| parent_is_section zipper = (Error "An unescaped variable may not be a immediate child of a section" r):errs
	| otherwise = errs
unescaped_pos _ errs = errs

-- Partials must be the single child of an XMLTag
partial_only_child r@R.PartialSelector{next=(Just something)} errs =
	(Error "A partial must be the only child of a node" r):errs
partial_only_child r@R.PartialSelector{prev=(Just something)} errs =
	(Error "A partial must be the only child of a node" r):errs
partial_only_child r@R.PartialSelector{..} errs
	| parent_is_section zipper = (Error "A partial may not be a immediate child of a section" r):errs
	| otherwise = errs
partial_only_child _ errs = errs

parent_is_section :: R.Zipper -> Bool
parent_is_section (_, (R.Crumb _ P.Section{} _):_) = True
parent_is_section _ = False

-- Lookahead is not supported yet
no_lookahead resolution@R.SectionSelector{..} errs
	| is_mustache prev = (Error "The previous node is mustache, lookahead is not supported yet" resolution):errs
	| is_mustache next = (Error "The next node is mustache, lookahead is not supported yet" resolution):errs
	| is_mustache first_c = (Error "The first child is mustache, lookahead is not supported yet" resolution):errs
	| is_mustache last_c = (Error "The last child is mustache, lookahead is not supported yet" resolution):errs
	| otherwise = errs
no_lookahead resolution@R.VariableSelector{..} errs
	| is_mustache prev = (Error "The previous node is mustache, lookahead is not supported yet" resolution):errs
	| is_mustache next = (Error "The next node is mustache, lookahead is not supported yet" resolution):errs
	| otherwise = errs
no_lookahead _ errs = errs

is_mustache (Just P.Section{}) = True
is_mustache (Just P.Partial{}) = True
is_mustache (Just P.Variable{}) = True
is_mustache _ = False

-- Ambiguous boundaries
ambiguous_boundaries resolution@R.SectionSelector{..} errs
	| ambiguous first_c next = (Error "The first child and next node are indistinguishable" resolution):errs
	| otherwise = errs
ambiguous_boundaries _ errs = errs

ambiguous :: Maybe P.Content -> Maybe P.Content -> Bool
ambiguous (Just a) (Just b) = (is_ambiguous a b) || (is_ambiguous b a)
ambiguous Nothing (Just b) = False
ambiguous (Just a) Nothing = False
ambiguous Nothing Nothing = True

is_ambiguous :: P.Content -> P.Content -> Bool
-- We don't do proper lookaheads yet, so sections and variables are considered ambiguous
is_ambiguous tag1@P.XMLTag{} tag2@P.XMLTag{} = (P.name tag1) == (P.name tag2)
is_ambiguous tag1@P.XMLTag{} tag2@P.EmptyXMLTag{} = (P.name tag1) == (P.name tag2)
is_ambiguous tag1@P.EmptyXMLTag{} tag2@P.EmptyXMLTag{} = (P.name tag1) == (P.name tag2)
is_ambiguous P.XMLComment{} P.XMLComment{} = True
is_ambiguous text1@P.Text{} text2@P.Text{} = t1 == (take (length t1) (P.text text2)) where t1 = P.text text1
is_ambiguous _ _ = False

-- Path with errors
path_with_errors valid_rs resolutions@(r:rs) errs =
	case find_path_error errs r of
		Just err -> path_with_errors [] (valid_rs ++ rs) (err:errs)
		Nothing  -> path_with_errors (r:valid_rs) rs errs
path_with_errors valid_rs [] errs = errs

find_path_error errs r =
	case get_path_top (R.path r) of
		top@R.Offset{..} ->
			case find_err errs offset of
				Just Error{..} -> Just (Unresolved r resolution)
				Just Unresolved{..} -> Just (Unresolved r cause)
				Nothing -> Nothing
		top@R.Child{..} ->
			case find_err errs offset of
				Just Error{..} -> Just (Unresolved r resolution)
				Just Unresolved{..} -> Just (Unresolved r cause)
				Nothing -> Nothing
		_ -> Nothing

get_path_top :: R.Path -> R.Path
get_path_top (R.Index i p) = get_path_top p
get_path_top (R.Attribute n p) = get_path_top p
get_path_top top = top

has_error :: Errors -> R.Resolution -> Bool
has_error errors r =
	case find_err errors r of
		Just err -> True
		Nothing -> False

find_err :: Errors -> R.Resolution -> Maybe Error
find_err ((err@Error{resolution}):errs) needle
	| resolution == needle = Just err
	| otherwise = find_err errs needle
find_err ((err@Unresolved{resolution}):errs) needle
	| resolution == needle = Just err
	| otherwise = find_err errs needle
find_err ((err@Warning{}):errs) needle = find_err errs needle
find_err [] needle = Nothing
