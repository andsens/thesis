{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Comb.Filter (
	filter_resolutions
) where
import qualified Comb.Parser as P
import qualified Comb.Resolver as R
import Text.Parsec.Pos(sourceLine,sourceName)
import Debug.Trace

type Warnings = [Warning]
type Errors = [Error]

data Warning = Warning {warn_msg :: String, warn_res :: R.Resolution}
data Error   = Error {err_msg :: String, err_res :: R.Resolution}


instance Show Warning where
	show (Warning message res) = (location res) ++ "\n" ++ message

instance Show Error where
	show (Error message res) = (location res) ++ "\n" ++ message

location r@R.VariableSelector{..} =
	"Variable " ++ (P.name node) ++ " in " ++ filename ++ ":" ++ line
	where
		filename = (sourceName . P.begin) node
		line = show $ (sourceLine . P.begin) node
location r@R.SectionSelector{..} =
	"Section " ++ (P.name node) ++ " in " ++ filename ++ ":" ++ line
	where
		filename = (sourceName . P.begin) node
		line = show $ (sourceLine . P.begin) node



filter_resolutions :: R.Resolutions -> (R.Resolutions, Warnings, Errors)
filter_resolutions resolutions =
	let (warnings, errors) = run_checks resolutions ([], []) checks
	in (filter (not . has_error errors) resolutions, warnings, errors)

run_checks resolutions set (check:checks) = run_checks resolutions (check resolutions set) checks
run_checks resolutions set [] = set

checks = [
	  run_check unescaped_offset
	--, run_check unescaped_is_encapsulated
	--, check if two variables in the same text section
	--, check if a section has prev and first == 'text' or next and last == 'text' => warning
	, path_with_errors []
	]

run_check check (r:rs) set = run_check check rs (check r set)
run_check check [] set = set

-- Unescaped offsets
unescaped_offset resolution set@(warns, errs) =
	case get_path_top (R.path resolution) of
		R.Offset _ (R.VariableSelector{node=P.Variable{escaped=False}}) ->
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

-- Path with errors
path_with_errors valid_rs resolutions@(r:rs) set@(warns, errs) =
	case find_path_error errs r of
		Just err -> path_with_errors [] (valid_rs ++ rs) (warns, err:errs)
		Nothing  -> path_with_errors (r:valid_rs) rs set
path_with_errors valid_rs [] set = set

find_path_error errs r =
	case get_path_top (R.path r) of
		top@R.Offset{..} ->
			if has_error errs offset
				then Just (Error "Unresolved offset found in path" r)
				else Nothing
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
