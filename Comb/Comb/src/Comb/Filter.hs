{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Comb.Filter (
	filter_resolutions
) where
import qualified Comb.Parser as P
import qualified Comb.Resolver as R
import Text.Parsec.Pos(sourceLine,sourceName)
import Data.List(sort)
import Debug.Trace

type Warnings = [Warning]
type Errors = [Error]

data Warning = Warning {warn_msg :: String, warn_res :: R.Resolution} deriving (Eq)
data Error   = Error {err_msg :: String, err_res :: R.Resolution} deriving (Eq)

instance Show Warning where
	show (Warning message res) = "Warning: " ++ (location res) ++ "\n    " ++ message
instance Show Error where
	show (Error message res) = "Error: " ++ (location res) ++ "\n    " ++ message

instance Ord Warning where
	compare x y = compare (warn_res x) (warn_res y)
instance Ord Error where
	compare x y = compare (err_res x) (err_res y)

location r@R.VariableSelector{..} =
	var_desc ++ " '" ++ (P.name node) ++ "' in " ++ filename ++ ":" ++ line
	where
		filename = (sourceName . P.begin) node
		line = show $ (sourceLine . P.begin) node
		var_desc = if (P.escaped node) then "escaped variable" else "unescaped variable"
location r@R.SectionSelector{..} =
	section_desc ++ " '" ++ (P.name node) ++ "' in " ++ filename ++ ":" ++ line
	where
		filename = (sourceName . P.begin) node
		line = show $ (sourceLine . P.begin) node
		section_desc = if (P.inverted node) then "inverted section" else "section"



filter_resolutions :: R.Resolutions -> (R.Resolutions, Warnings, Errors)
filter_resolutions resolutions =
	let (warnings, errors) = run_checks resolutions ([], []) checks
	in (filter (not . has_error errors) resolutions, sort warnings, sort errors)

run_checks resolutions set (check:checks) = run_checks resolutions (check resolutions set) checks
run_checks resolutions set [] = set

checks = [
	  run_check unescaped_offset
	--, run_check empty_section
	--, run_check unescaped_is_encapsulated
	--, check if two variables in the same text section
	--, check if a section has prev and first == 'text' or next and last == 'text' => warning
	, run_check ambiguous_boundaries
	, path_with_errors []
	]

run_check check (r:rs) set = run_check check rs (check r set)
run_check check [] set = set

-- Unescaped offsets
unescaped_offset resolution set@(warns, errs) =
	case get_path_top (R.path resolution) of
		R.Offset (R.VariableSelector{node=P.Variable{escaped=False}}) ->
			(warns, (Error "Path contains unescaped variable" resolution):errs)
		_ -> set

---- Unescaped variable is always encapsulated
--unescaped_at_end R.VariableSelector{node=P.Variable{escaped=False},zipper=(crumb, trail),..} set@(warns, errs) =
--	R.r
--	case path of
--		R.Index 0 p -> (warns, errs)
--		_ -> (warns, (Error "Path contains unescaped variable" resolution):errs)
--		(warns, (Error "Path contains unescaped variable" resolution):errs)
--	_ -> set

-- Unescaped offsets
ambiguous_boundaries resolution@R.SectionSelector{..} set@(warns, errs)
	| ambiguous first_c next = (warns, (Error "The first child and next node are indistinguishable" resolution):errs)
	-- | ambiguous last_c first_c = (warns, (Error "The last child and next node are indistinguishable" resolution):errs)
	| otherwise = set
ambiguous_boundaries _ set = set

ambiguous :: Maybe P.Content -> Maybe P.Content -> Bool
ambiguous (Just a) (Just b) = (is_ambiguous a b) || (is_ambiguous b a)
ambiguous Nothing (Just b) = False
ambiguous (Just a) Nothing = False
ambiguous Nothing Nothing = True

is_ambiguous :: P.Content -> P.Content -> Bool
is_ambiguous P.Section{} P.Section{} = True
is_ambiguous P.Variable{} P.Variable{} = True
is_ambiguous P.Section{} P.Variable{} = True
is_ambiguous tag1@P.XMLTag{} tag2@P.XMLTag{} = (P.name tag1) == (P.name tag2)
is_ambiguous tag1@P.EmptyXMLTag{} tag2@P.EmptyXMLTag{} = (P.name tag1) == (P.name tag2)
is_ambiguous tag1@P.XMLTag{} tag2@P.EmptyXMLTag{} = (P.name tag1) == (P.name tag2)
is_ambiguous P.XMLComment{} P.XMLComment{} = True
is_ambiguous text1@P.Text{} text2@P.Text{} = t1 == (take (length t1) (P.text text2)) where t1 = P.text text1
is_ambiguous P.Text{} P.Section{} = True
is_ambiguous P.Text{} P.Variable{} = True
is_ambiguous _ _ = False


-- Path with errors
path_with_errors valid_rs resolutions@(r:rs) set@(warns, errs) =
	case find_path_error errs r of
		Just err -> path_with_errors [] (valid_rs ++ rs) (warns, err:errs)
		Nothing  -> path_with_errors (r:valid_rs) rs set
path_with_errors valid_rs [] set = set

find_path_error errs r =
	case get_path_top (R.path r) of
		top@R.Offset{..} ->
			case find_err errs offset of
				Just Error{..} -> Just (Error ("Unresolved offset from '" ++ (P.name (R.node err_res)) ++ "' found in path") r)
				Nothing -> Nothing
		top@R.Child{..} ->
			case find_err errs offset of
				Just Error{..} -> Just (Error ("Unresolved child from '" ++ (P.name (R.node err_res)) ++ "' found in path") r)
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
find_err (err:errs) r
	| (R.node r) == R.node (err_res err) = Just err
	| otherwise          = find_err errs r
find_err [] r = Nothing
