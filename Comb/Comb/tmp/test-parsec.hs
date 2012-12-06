module Main where
import Text.ParserCombinators.Parsec

run :: Show a => Parser a -> String -> IO ()
run p input =
	case (parse p "" input) of
		Left err -> do { putStr "parse error at "; print err }
		Right x  -> print x

openClose :: Parser Char
openClose = do{ char '('
              ; char ')'
              }

parens  :: Parser ()
parens  = do{ char '('
            ; parens
            ; char ')'
            ; parens
            }
        <|> return ()

simple :: Parser Char
simple  = letter
