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

lexer  = T.makeTokenParser (
	emptyDef {
		T.commentStart    = "{{!",
		T.commentEnd      = "}}",
		T.opLetter        = oneOf "/",
		T.reservedOpNames = x_start_ops ++ x_end_ops ++ m_start_ops ++ m_end_ops,
		T.caseSensitive   = False
	})

identifier    = T.identifier lexer
symbol        = T.symbol lexer
reservedOp    = T.reservedOp lexer

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
	reservedOp "{{" <?> "mustache variable"
	name <- identifier
	reservedOp "}}"
	return $ Variable name True
m_unescaped = do
	reservedOp "{{{" <?> "mustache variable (unescaped)"
	name <- identifier
	reservedOp "}}}"
	return $ Variable name False

m_section allowed_content = do
	inverted <-
		do
			reservedOp "{{#" <?> "mustache section"
			return False
		<|> do
			reservedOp "{{^" <?> "mustache section (inverted)"
			return True
	name <- identifier
	reservedOp "}}"
	content <- many allowed_content
	reservedOp "{{/"; C.string name; reservedOp "}}"
	return (Section name inverted content)


comment_content =
	    m_section comment_content
	<|> m_unescaped
	<|> m_escaped
	<|> text_data ("-->":m_start_ops)

xml_comment =
	do
		reservedOp "<!--"
		content <- many comment_content
		reservedOp "-->"
		return $ XMLComment content

xml_tag = do
	reservedOp "<"
	name <- identifier
	attrs <- many xml_attribute
	content <-
		do 
			reservedOp "/>"
			return []
		<|> do
			reservedOp ">"
			content <- many any_content
			reservedOp "</"
			C.string name
			reservedOp ">"
			return content
	return (XMLTag name attrs content)

string_content =
	    m_section string_content
	<|> m_unescaped
	<|> m_escaped
	<|> text_data ("\"":m_start_ops)

xml_attribute = do
	name <- identifier
	symbol "="
	value <- between (symbol "\"") (symbol "\"") (many string_content)
	return $ XMLAttribute name value

text_data disallowed_operators =
	do
		let ops = map C.string disallowed_operators
		notFollowedBy (choice ops)
		first <- anyChar
		rest <- manyTill anyChar ( eof <|> do { choice . map lookAhead $ ops; return () } )
		return $ Text (first:rest)







