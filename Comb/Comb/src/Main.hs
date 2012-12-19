{-# LANGUAGE DeriveDataTypeable, NamedFieldPuns, RecordWildCards #-}
import System.Console.CmdArgs
import System.IO(hPutStrLn, stderr, writeFile)
import Comb.Parser(parse_file)
import Comb.Resolver(resolve)
import Comb.Filter(filter_resolutions)
import Comb.Generator(generate)
import Text.JSON.Pretty(pp_array)
import Text.JSON(encodeStrict)

data CombArgs = CombArgs {
	  files :: [FilePath]
	, out :: String
	, pretty :: Bool
} deriving (Data, Typeable, Show, Eq)

arguments = CombArgs {
	  files = def &= args &= typFile
	, out = def &= typ "FILE" &= help "Output file"
	, pretty = def  &= help "Pretty print JSON"
} &=
	program "comb-gen" &=
	help "Generate DOM selectors for mustache template variables" &=
	summary "Comb for Mustache v0.0.0, (C) Anders Ingemann" &=
	details ["Comb for Mustache creates various selectors for each variable in a mustache template."]

main = do
	args <- cmdArgs arguments
	let outfile = out args
	let templates = filter ((`elem` ["mustache", "mst"]) . extension) (files args)
	let pp = pretty args
	case templates of
		[]          -> error "No files given"
		template:[] -> mapM (run (comb pp) outfile) templates
		templates   -> mapM (run (comb pp) "") templates
	return ()

run comb outfile infile = do
	string <- comb infile
	case outfile of
		"-"     -> putStrLn string
		""      -> writeFile (infile ++ "-comb") string
		outfile -> writeFile (outfile) string

comb pretty filename = do
	ast <- parse_file filename
	--mapM_ print ast
	let unfiltered_resolutions = resolve ast
	let (resolutions, errors) = filter_resolutions unfiltered_resolutions
	let json = generate resolutions
	let output = if pretty then show (pp_array json) else encodeStrict json
	mapM_ (hPutStrLn stderr . show) errors
	return output

extension = reverse . takeWhile (/= '.') . reverse
