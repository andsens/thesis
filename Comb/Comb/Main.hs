{-# LANGUAGE DeriveDataTypeable, NamedFieldPuns, RecordWildCards #-}
module Comb.Main
where
import System.Console.CmdArgs
import System.IO
import Comb.Parser(parse_file)
import Comb.Resolver(resolve)
import Comb.Filter(filter_resolutions)
import Comb.Generator(generate)
import Text.JSON.Pretty(pp_js_object)
import Debug.Trace

data CombArgs = CombArgs {
	files :: [FilePath]
	--, output :: String
} deriving (Data, Typeable, Show, Eq)

arguments = CombArgs {
	files = def &= args &= typFile
	--, output = def &= typ "DIR|FILE" &= help "Output directory or file"
} &=
	--verbosity &=
	program "comb-gen" &=
	help "Generate DOM selectors for mustache template variables" &=
	summary "Comb for Mustache v0.0.0, (C) Anders Ingemann" &=
	details ["Comb for Mustache creates various selectors for each variable in a mustache template."]

main = do
	args <- cmdArgs arguments
	ast <- parse_file (head $ files args)
	let unfiltered_resolutions = resolve ast
	let (resolutions, warnings, errors) = filter_resolutions unfiltered_resolutions
	let json = (generate resolutions)
	putStrLn $ show (pp_js_object json)
	mapM_ (hPutStrLn stderr . show) warnings
	mapM_ (hPutStrLn stderr . show) errors
