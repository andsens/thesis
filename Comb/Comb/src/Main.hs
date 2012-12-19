{-# LANGUAGE DeriveDataTypeable, NamedFieldPuns, RecordWildCards #-}
import System.Console.CmdArgs
import System.IO(hPutStrLn, stderr, writeFile)
import Comb.Parser(parse_file)
import Comb.Resolver(resolve)
import Comb.Filter(filter_resolutions)
import Comb.Generator(generate)
import Text.JSON.Pretty(pp_js_object)

data CombArgs = CombArgs {
	  infile :: FilePath
	, outfile :: String
} deriving (Data, Typeable, Show, Eq)

arguments = CombArgs {
	  infile = def &= args &= typFile
	, outfile = def &= typ "FILE" &= help "Output file"
} &=
	--verbosity &=
	program "comb-gen" &=
	help "Generate DOM selectors for mustache template variables" &=
	summary "Comb for Mustache v0.0.0, (C) Anders Ingemann" &=
	details ["Comb for Mustache creates various selectors for each variable in a mustache template."]

main = do
	args <- cmdArgs arguments
	let filename = infile args
	ast <- parse_file filename
	let unfiltered_resolutions = resolve ast
	let (resolutions, errors) = filter_resolutions unfiltered_resolutions
	let json = show (pp_js_object (generate resolutions))
	case (outfile args) of
		""   -> writeFile (filename ++ "-comb") json
		"-"  -> putStrLn json
		name -> writeFile name json
	mapM_ (hPutStrLn stderr . show) errors
