{-# LANGUAGE DeriveDataTypeable #-}
module Comb.Main
where
import System.Console.CmdArgs
import Comb.Parser(template)
import Text.Parsec(runParser)
import Comb.Resolver
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
	resolutions <- comb_file (head $ files args)
	case resolutions of
		Just res -> mapM_ print (filter (\x -> case x of Warning{} -> True; _ -> False ) $ reverse res)
		Nothing -> return ()

comb_file (path) = do
	input <- readFile path
	case (runParser template () path input) of
		Left err ->  do
			putStr "parse error at "
			print err
			return Nothing
		Right ast -> do
			return $ Just (resolve ast)

--case (runParser parser () path input) of
--		Left err -> do
--			putStr "parse error at "
--			print err
--		Right x  -> print x
