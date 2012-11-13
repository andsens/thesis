{-# LANGUAGE DeriveDataTypeable #-}
import System.Console.CmdArgs


data SelectorGenerator = SelectorGenerator {
	files :: [FilePath],
	encoding :: String
} deriving (Data, Typeable, Show, Eq)

arguments = SelectorGenerator {
	files = def &= args &= typFile,
	encoding = def &= typ "ENC" &= opt "utf8" &= help "Choose the text encoding"
} &=
	--verbosity &=
	program "selector-gen" &=
	help "Generate DOM selectors for mustache template variables" &=
	summary "Selector Generator v0.0.0, (C) Anders Ingemann" &=
	details ["Selector Generator creates various selectors for each variable in a mustache template."]

main = do
	x <- cmdArgs arguments
	print x
