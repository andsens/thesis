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
	  infile :: FilePath
	, outfile :: String
	, pretty :: Bool
} deriving (Data, Typeable, Show, Eq)

arguments = CombArgs {
	  infile = def &= args &= typFile
	, outfile = def &= typ "FILE" &= help "Output file"
	, pretty = def  &= help "Pretty print JSON"
} &=
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
	let json = generate resolutions
	let output = if pretty args then show (pp_array json) else encodeStrict json
	case (outfile args) of
		""   -> writeFile (filename ++ "-comb") output
		"-"  -> putStrLn output
		name -> writeFile name output
	mapM_ (hPutStrLn stderr . show) errors
