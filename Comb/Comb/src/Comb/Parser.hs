{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Comb.Parser (
	Content(..),
	Comb.Parser.parse,
	parse_file
) where
import Text.Parsec as Parsec
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Char as C
import Text.Parsec.Language(emptyDef)
import Text.Parsec.Combinator
import Text.Parsec.Pos(SourcePos)
import Text.HTML.TagSoup.Entity(lookupEntity)

parse :: String -> Either ParseError [Content]
parse input = Parsec.parse template "" input

parse_file :: String -> IO [Content]
parse_file path = do
	input <- readFile path
	case (runParser template () path input) of
		Left err -> do
			putStr "parse error at "
			error (show err)
		Right ast ->
			return ast

x_start_ops = ["<", "</", "<!--"]
x_end_ops   = [">", "/>", "-->"]

xml_lexer = T.makeTokenParser (
	emptyDef {
		T.commentStart    = "{{!",
		T.commentEnd      = "}}",
		T.identStart      = letter <|> oneOf "_-:",
		T.identLetter     = letter <|> digit <|> oneOf "._-:",
		T.opLetter        = oneOf "/",
		T.reservedOpNames = x_start_ops ++ x_end_ops,
		T.caseSensitive   = False
	})

x_identifier = T.identifier xml_lexer
x_symbol     = T.symbol xml_lexer
x_reservedOp = T.reservedOp xml_lexer

m_opLetters = "#/^>{!"
m_start_ops = "{{" : ( map ( ("{{"++) . (:"") ) m_opLetters )
m_end_ops   = ["}}", "}}}"]

mustache_lexer  = T.makeTokenParser (
	emptyDef {
		T.commentStart    = "{{!",
		T.commentEnd      = "}}",
		T.identStart      = letter <|> oneOf "_",
		T.identLetter     = alphaNum <|> oneOf "_.",
		T.opLetter        = oneOf m_opLetters,
		T.reservedOpNames = m_start_ops ++ m_end_ops,
		T.caseSensitive   = False
	})

m_identifier = T.identifier mustache_lexer
m_reservedOp = T.reservedOp mustache_lexer

{--
content ::= {{  id  }}
					| {{{ id }}}
					| {{# id  }} content {{/ id }}
					| {{^ id  }} content {{/ id }}
					| <id [id = attribute_content]* />
					| <id [id = attribute_content]* > content </id>
					| <!-- comment_content -->
					| string
--}

template = many any_content

any_content =
			m_section any_content
	<|> m_unescaped
	<|> m_escaped
	<|> xml_comment
	<|> xml_tag
	<|> text_data (x_start_ops ++ m_start_ops) entity_decoder


data Content =
	Section {
		name :: String,
		inverted :: Bool,
		contents :: [Content],
		begin :: SourcePos,
		end :: SourcePos
	} | Variable {
		name :: String,
		escaped :: Bool,
		begin :: SourcePos,
		end :: SourcePos
	} | XMLTag {
		name :: String,
		attributes :: [Content], -- will only ever be XMLAttribute, the parser ensures that
		contents :: [Content],
		begin :: SourcePos,
		end :: SourcePos
	} | EmptyXMLTag {
		name :: String,
		attributes :: [Content], -- will only ever be XMLAttribute, the parser ensures that
		begin :: SourcePos,
		end :: SourcePos
	} | XMLComment {
		contents :: [Content],
		begin :: SourcePos,
		end :: SourcePos
	} | XMLAttribute {
		name :: String,
		contents :: [Content],
		begin :: SourcePos,
		end :: SourcePos
	} | Text {
		text :: String,
		begin :: SourcePos,
		end :: SourcePos
	}
	deriving (Eq)

instance Ord Content where
	compare x y = compare (begin x) (begin y)

instance Show Content where
	show Section{inverted=False,..} = "(#"++name++")" ++ (concat $ map show contents) ++ "(/"++name++")"
	show Section{inverted=True,..} = "(^"++name++")" ++ (concat $ map show contents) ++ "(/"++name++")"
	show Variable{escaped=True,..} = "("++name++")"
	show Variable{escaped=False,..} = "(("++name++"))"
	show XMLTag{..} = "["++name++(concat $ map show attributes)++"]" ++ (concat $ map show contents) ++ "[/"++name++"]"
	show EmptyXMLTag{..} = "["++name++(concat $ map show attributes)++"/]"
	show XMLComment{..} = "[!--" ++ (concat $ map show contents) ++ "--]"
	show XMLAttribute{..} = " " ++ name ++ "=\"" ++ (concat $ map show contents) ++ "\""
	show Text{..} = text

m_unescaped = do
	begin <- getPosition
	m_reservedOp "{{{" <?> "mustache variable (unescaped)"
	name <- m_identifier
	C.string "}}}"
	end <- getPosition
	return $ Variable name False begin end

m_escaped = do
	begin <- getPosition
	m_reservedOp "{{" <?> "mustache variable"
	notFollowedBy (oneOf m_opLetters)
	name <- m_identifier
	C.string "}}"
	end <- getPosition
	return $ Variable name True begin end

m_section allowed_content = do
	begin <- getPosition
	inverted <- do
			m_reservedOp "{{#" <?> "mustache section"
			return False
		<|> do
			m_reservedOp "{{^" <?> "mustache section (inverted)"
			return True
	name <- m_identifier
	C.string "}}"
	contents <- many allowed_content
	m_reservedOp "{{/"
	C.string name
	C.string "}}"
	end <- getPosition
	return $ Section name inverted contents begin end


xml_tag = do
	begin <- getPosition
	x_reservedOp "<" <?> "tag"
	name <- x_identifier
	attrs <- many xml_attribute
	tag <- do
			C.string "/>"
			end <- getPosition
			return $ EmptyXMLTag name attrs begin end
		<|> do
			C.string ">"
			contents <- many any_content
			x_reservedOp "</" <?> "closing tag"
			C.string name
			C.string ">"
			end <- getPosition
			return $ XMLTag name attrs contents begin end
	return tag

attribute_content =
			m_section attribute_content
	<|> m_unescaped
	<|> m_escaped
	<|> text_data ("\"":m_start_ops) id

xml_attribute = do
	begin <- getPosition
	name <- x_identifier
	x_symbol "="
	value <- between (C.string "\"") (x_symbol "\"") (many attribute_content)
	end <- getPosition
	return $ XMLAttribute name value begin end


comment_content =
			m_section comment_content
	<|> m_unescaped
	<|> m_escaped
	<|> text_data ("-->":m_start_ops) id

xml_comment = do
	begin <- getPosition
	try (C.string "<!--")
	contents <- many comment_content
	C.string "-->"
	end <- getPosition
	return $ XMLComment contents begin end

text_data disallowed_operators decoder = do
	let ops = map C.string disallowed_operators
	notFollowedBy (choice ops)
	begin <- getPosition
	first <- anyChar
	rest <- manyTill anyChar ( eof <|> do { choice . map (try . lookAhead) $ ops; return () } )
	end <- getPosition
	let string = decoder (first:rest)
	return $ Text string begin end

entity_decoder ('&':string) = 
	case lookupEntity (takeWhile (';' /=) string) of
		Just char -> char:rest
		Nothing -> rest
	where rest = drop 1 $ dropWhile (';' /=) string
entity_decoder (char:string) = char:(entity_decoder string)
entity_decoder [] = []
