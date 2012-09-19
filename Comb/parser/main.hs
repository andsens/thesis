{-# LANGUAGE DeriveDataTypeable #-}
import System.Console.CmdArgs
import System.IO
import Text.XML.HXT.Parser.XmlParsec as XmlParsec

data JqselGen = JqselGen {
	file :: [FilePath]
	--verbose :: Bool
	}
	deriving (Data,Typeable,Show,Eq)

jqselGen = cmdArgsMode $ JqselGen
	{file = def
		&= argPos 0
		&= opt "template.mustache"
		&= typFile &= help "Input file to generate jQuery selectors for"
	--,verbose = def &= name "v" &= name "verbose" &= help "Print more info"
	} &=
	verbosity &=
	help "Create jQuery selectors for placeholders in mustache templates" &=
	summary "jqselGen v0.0.0, (C) Anders Ingemann" &=
	details ["Details"]

main = do
	x <- cmdArgsRun jqselGen
	print x


--main = do
--	handle <- openFile 
--	XmlParsec.xread
--	what <- getLine
--	putStrLn (what ++ "? Sounds boring...")
