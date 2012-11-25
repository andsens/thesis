module Mustache where
import Text.Parsec
import Text.Parsec.Prim
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Char as C
import Text.Parsec.Language(emptyDef)
import Text.Parsec.Combinator
--import CustomCombinators
import Debug.Trace

--run :: Show a => Parsec a -> String -> IO ()
run parser input =
	case (parse parser "" input) of
		Left err -> do
			putStr "parse error at ";
			print err
		Right x  -> print x

file parser path = do
	input <- readFile path
	return (runParser parser () path input)


x_start_ops = ["<", "</"]
x_end_ops   = [">", "/>"]
m_start_ops = ["{{", "{{/", "{{{", "{{#", "{{^", "{{!", "{{>"]
m_end_ops   = ["}}", "}}}"]

lexer :: T.TokenParser ()
lexer  = T.makeTokenParser (
	emptyDef {
		T.opLetter        = oneOf "/=",
		T.reservedOpNames = x_start_ops ++ x_end_ops ++ m_start_ops ++ m_end_ops,
		T.caseSensitive   = False
	})

identifier    = T.identifier lexer
symbol        = T.symbol lexer
reservedOp    = T.reservedOp lexer
operator      = T.operator lexer
stringLiteral = T.stringLiteral lexer

{--
content ::= {{  id  }}
          | {{{ id }}}
          | {{# id  }} content {{/ id }}
          | {{^ id  }} content {{/ id }}
          | <id [id = str_content]* />
          | <id [id = str_content]* > content </id>
          | string
--}

template = many any_content

any_content =
	    try m_escaped
	<|> try m_unescaped
	<|> try m_content_section
	<|> try xml_empty_tag
	<|> try xml_tag
	<|> try node_data

str_content =
	    try m_escaped
	<|> try m_unescaped
	<|> try m_string_section
	<|> try string_data

data Content =
	Section {
		name :: String,
		inverted :: Bool,
		content :: [Content]
	} | Variable {
		name :: String,
		escaped :: Bool
	} | XMLTag {
		name :: String,
		attributes :: [XMLAttribute],
		content :: [Content]
	} | Text {
		text :: String
	} deriving (Show)

data XMLAttribute =
	XMLAttribute {
		attr_name :: String,
		attr_content :: [Content]
	} deriving (Show)

m_escaped   = do { reservedOp  "{{"; name <- identifier; reservedOp "}}";  return $ Variable name True }
m_unescaped = do { reservedOp "{{{"; name <- identifier; reservedOp "}}}"; return $ Variable name False }

m_content_section = do
	inverted <- ( do { try $ reservedOp "{{#"; return False }
	          <|> do { try $ reservedOp "{{^"; return True } )
	name <- identifier
	reservedOp "}}"
	content <- many any_content
	reservedOp "{{/"; C.string name; reservedOp "}}"
	return (Section name inverted content)

m_string_section = do
	inverted <- ( do { try $ reservedOp "{{#"; return False }
	          <|> do { try $ reservedOp "{{^"; return True } )
	name <- identifier
	reservedOp "}}"
	content <- many str_content
	reservedOp "{{/"; C.string name; reservedOp "}}"
	return (Section name inverted content)

xml_empty_tag = do
	reservedOp "<";
	tag_name <- identifier
	attrs <- xml_attribute `sepBy` (many1 C.space)
	reservedOp "/>";
	return (XMLTag tag_name attrs [])

xml_tag = do
	reservedOp "<";
	tag_name <- identifier
	attrs <- xml_attribute `sepBy` (many1 C.space)
	reservedOp ">";
	content <- many any_content
	reservedOp "</"; C.string tag_name; reservedOp ">"
	return (XMLTag tag_name attrs content)

xml_attribute = do
	name <- identifier
	symbol "="
	value <- between (symbol "\"") (symbol "\"") (many str_content)
	return $ XMLAttribute name value

node_data =
	do
		let ops = map C.string (x_start_ops ++ m_start_ops)
		notFollowedBy (choice ops)
		first <- anyChar
		rest <- manyTill anyChar ( eof <|> do { choice . map lookAhead $ ops; return () } )
		return $ Text (first:rest)

string_data =
	do
		let ops = map C.string (x_start_ops ++ m_start_ops)
		notFollowedBy (choice ops)
		first <- anyChar
		rest <- manyTill anyChar ( choice . map lookAhead $ (C.string "\""):ops )
		return $ Text (first:rest)








