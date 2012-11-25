module Mustache where
import Text.Parsec
import Text.Parsec.Prim
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Char as C
import Text.Parsec.Language(emptyDef)
import Text.Parsec.Combinator
import Data.Functor.Identity(Identity)
import Debug.Trace

run parser input =
	case (parse parser "" input) of
		Left err -> do
			putStr "parse error at ";
			print err
		Right x  -> print x

file parser path = do
	input <- readFile path
	case (runParser parser () path input) of
		Left err -> do
			putStr "parse error at ";
			print err
		Right x  -> print x
	return ()


x_start_ops = ["<", "</", "<!--"]
x_end_ops   = [">", "/>", "-->"]
m_start_ops = ["{{", "{{/", "{{{", "{{#", "{{^", "{{!", "{{>"]
m_end_ops   = ["}}", "}}}"]

xml_lexer = T.makeTokenParser (
	emptyDef {
		T.nestedComments  = False,
		T.opStart         = oneOf "<",
		T.opLetter        = oneOf "/",
		T.reservedOpNames = x_start_ops ++ x_end_ops,
		T.caseSensitive   = False
	})

x_identifier    = T.identifier xml_lexer
x_symbol        = T.symbol xml_lexer
x_reservedOp    = T.reservedOp xml_lexer

mustache_lexer = T.makeTokenParser (
	emptyDef {
		T.commentStart    = "{{!",
		T.commentEnd      = "}}",
		T.nestedComments  = False,
		T.opStart         = oneOf "",
		T.opLetter        = oneOf "",
		--T.reservedOpNames = m_start_ops ++ m_end_ops,
		T.caseSensitive   = False
	})

m_identifier    = T.identifier mustache_lexer
m_reservedOp    = T.reservedOp mustache_lexer
m_symbol        = T.symbol mustache_lexer

{--
content ::= {{  id  }}
          | {{{ id }}}
          | {{# id  }} content {{/ id }}
          | {{^ id  }} content {{/ id }}
          | <id [id = string_content]* />
          | <id [id = string_content]* > content </id>
          | <!-- comment -->
          | string
--}

template = many any_content

any_content =
	    m_section any_content
	<|> m_unescaped
	<|> m_escaped
	<|> xml_comment
	<|> xml_tag
	<|> text_data (x_start_ops ++ m_start_ops)

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
	} | XMLComment {
		content :: [Content]
	} | Text {
		text :: String
	} deriving (Show)

data XMLAttribute =
	XMLAttribute {
		attr_name :: String,
		attr_content :: [Content]
	} deriving (Show)

m_escaped   = do
	m_reservedOp "{{" <?> "mustache variable"
	name <- m_identifier
	m_reservedOp "}}"
	return $ Variable name True
m_unescaped = do
	m_reservedOp "{{{" <?> "mustache variable (unescaped)"
	name <- m_identifier
	m_reservedOp "}}}"
	return $ Variable name False

m_section allowed_content = do
	inverted <-
		do
			m_reservedOp "{{#" <?> "mustache section"
			return False
		<|> do
			m_reservedOp "{{^" <?> "mustache section (inverted)"
			return True
	name <- m_identifier
	m_reservedOp "}}"
	content <- many allowed_content
	m_reservedOp "{{/"; C.string name; m_reservedOp "}}"
	return (Section name inverted content)


comment_content =
	    m_section comment_content
	<|> m_unescaped
	<|> m_escaped
	<|> text_data ("-->":m_start_ops)

xml_comment =
	do
		x_reservedOp "<!--"
		content <- many comment_content
		x_reservedOp "-->"
		return $ XMLComment content

xml_tag = do
	x_reservedOp "<"
	name <- x_identifier
	attrs <- many xml_attribute
	content <-
		do 
			x_reservedOp "/>"
			return []
		<|> do
			x_reservedOp ">"
			content <- many any_content
			x_reservedOp "</"
			C.string name
			x_reservedOp ">"
			return content
	return (XMLTag name attrs content)

string_content =
	    m_section string_content
	<|> m_unescaped
	<|> m_escaped
	<|> text_data ("\"":m_start_ops)

xml_attribute = do
	name <- x_identifier
	x_symbol "="
	value <- between (x_symbol "\"") (x_symbol "\"") (many string_content)
	return $ XMLAttribute name value

text_data disallowed_operators =
	do
		let ops = map C.string disallowed_operators
		notFollowedBy (choice ops)
		first <- anyChar
		rest <- manyTill anyChar ( eof <|> do { choice . map lookAhead $ ops; return () } )
		return $ Text (first:rest)







