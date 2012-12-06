{-# LANGUAGE DeriveDataTypeable, NamedFieldPuns, RecordWildCards #-}
module Comb.Main
where
import System.Console.CmdArgs
import Comb.Parser(parse_file)
import Comb.Resolver(resolve)
import Comb.Generator(generate)
import Text.JSON.Pretty(pp_js_object)
import Debug.Trace

data CombArgs = CombArgs {
	files :: [FilePath]
	--, encoding :: String
} deriving (Data, Typeable, Show, Eq)

arguments = CombArgs {
	files = def &= args &= typFile
	--, encoding = def &= typ "ENC" &= opt "utf8" &= help "Choose the text encoding"
} &=
	--verbosity &=
	program "comb-gen" &=
	help "Generate DOM selectors for mustache template variables" &=
	summary "Comb for Mustache v0.0.0, (C) Anders Ingemann" &=
	details ["Comb for Mustache creates various selectors for each variable in a mustache template."]

main = do
	args <- cmdArgs arguments
	ast <- parse_file (head $ files args)
	--mapM_ print ast
	let resolutions = (resolve ast)
	--mapM_ print (reverse resolutions)
	let json = (generate resolutions)
	putStrLn $ show (pp_js_object json)
