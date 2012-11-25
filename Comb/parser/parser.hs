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

lexer :: T.TokenParser ()
lexer  = T.makeTokenParser (
	emptyDef {
		T.opLetter        = oneOf "/=",
		T.reservedOpNames = ["<", "</", "{{", "}}", "/>", ">"],
		T.caseSensitive   = False
	})

identifier    = T.identifier lexer
symbol        = T.symbol lexer
reservedOp    = T.reservedOp lexer
operator      = T.operator lexer
stringLiteral = T.stringLiteral lexer

x_start_ops = ["<", "</"]
x_end_ops   = [">", "/>"]
m_start_ops = ["{{", "{{/", "{{{", "{{#", "{{^", "{{!", "{{>"]
m_end_ops   = ["}}", "}}}"]

data Mustache =
	Section {
		name :: String,
		inverted :: Bool,
		content :: [Content]
	} | Variable {
		name :: String,
		escaped :: Bool
	} deriving (Show)

m_escaped   = do { reservedOp  "{{"; var_name <- identifier; reservedOp "}}";  return $ Variable var_name True }
m_unescaped = do { reservedOp "{{{"; var_name <- identifier; reservedOp "}}}"; return $ Unescaped var_name False }

m_content_section = do
	reservedOp "{{#"; var_name <- identifier; reservedOp "}}"
	--trees <- content
	reservedOp "{{/"; C.string var_name; reservedOp "}}"
	return (Section var_name False [])

m_content_inverted = do
	reservedOp "{{^"; var_name <- identifier; reservedOp "}}"
	--trees <- content
	reservedOp "{{/"; C.string var_name; reservedOp "}}"
	return (Section var_name True [])

mustache_content =
	    try m_content_section
	<|> try m_content_inverted
	<|> try m_escaped
	<|> try m_unescaped


data XMLAttribute =
	XMLAttribute {
		attr_name :: String,
		attr_content :: [StrContent]
	} deriving (Show)

xml_attribute = do
	name <- identifier
	symbol "="
	value <- stringLiteral  -- for now "between" maybe?
	return $ XMLAttribute name []

data XMLTag =
	XMLTag {
		name :: String,
		attributes :: [XMLAttribute],
		content :: [Content]
	} deriving (Show)

xml_empty_tag = do
	reservedOp "<";
	tag_name <- identifier
	attrs <- sepBy (many1 C.space) xml_attribute
	reservedOp "/>";
	return (XMLTag tag_name [] [])

xml_tag = do
	reservedOp "<";
	tag_name <- identifier
	attrs <- sepBy (many1 C.space) xml_attribute
	reservedOp ">";
	--trees <- content
	reservedOp "</"; C.string tag_name; reservedOp ">"
	return (XMLTag tag_name [] [])

xml_content = try xml_empty_tag <|> try xml_tag

data Content =
	  MustacheContent Mustache
	| XMLContent XMLTag
	| TextContent Text
	deriving (Show)


data Text = Data String deriving (Show)
node_data =
	do
		let ops = map C.string (x_start_ops ++ m_start_ops)
		notFollowedBy (choice ops)
		first <- anyChar
		rest <- manyTill anyChar (choice . map lookAhead $ ops)
		return $ Data (first:rest)
{--
content ::= {{  id  }}
          | {{{ id }}}
          | {{# id  }} content {{/ id }}
          | {{^ id  }} content {{/ id }}
          | <id [id = del_content]* />
          | <id [id = del_content]* > content </id>
          | string
--}
content = do
	    do {mst <- try mustache_content; return $ MustacheContent mst}
	<|> do {xml <- try xml_content; return $ XMLContent xml}
	<|> do {txt <- try node_data; return $ TextContent txt}

template = many content
