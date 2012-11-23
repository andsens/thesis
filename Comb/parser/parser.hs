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

data Text = Data String deriving (Show)
node_data =
	do
		let ops = map C.string (x_start_ops ++ m_start_ops)
		notFollowedBy (choice ops)
		first <- anyChar
		rest <- manyTill anyChar (choice . map lookAhead $ ops)
		return $ Data (first:rest)

data GenVariable =
	  Escaped String
	| Unescaped String
	| Comment String
	| Partial String
	deriving (Show)

m_escaped   = do { reservedOp  "{{"; var_name <- identifier; reservedOp "}}";  return $ Escaped var_name }
m_unescaped = do { reservedOp "{{{"; var_name <- identifier; reservedOp "}}}"; return $ Unescaped var_name }


data GenSection =
	  NormalSection String
	| InvertedSection String
	deriving (Show)

m_content_section = do
	trace "open" $ reservedOp "{{#"; var_name <- identifier; reservedOp "}}"
	--trees <- content
	reservedOp "{{/"; C.string var_name; reservedOp "}}"
	return (NormalSection var_name)

m_content_inverted = do
	reservedOp "{{^"; var_name <- identifier; reservedOp "}}"
	--trees <- content
	reservedOp "{{/"; C.string var_name; reservedOp "}}"
	return (InvertedSection var_name)


data Mustache =
	  Section GenSection
	| Variable GenVariable
	deriving (Show)

mustache_content =
	    do {section <- try m_content_section; return $ Section section}
	<|> do {section <- try m_content_inverted; return $ Section section}
	<|> do {variable <- try m_escaped; return $ Variable variable}
	<|> do {variable <- try m_unescaped; return $ Variable variable}


data XMLAttribute = XMLAttribute String deriving (Show)

xml_attribute = do
	name <- identifier
	symbol "="
	value <- stringLiteral  -- for now "between" maybe?
	return $ XMLAttribute name

data EmptyXMLTag = EmptyXMLTag String deriving (Show)

xml_empty_tag = do
	reservedOp "<";
	tag_name <- identifier
	attrs <- sepBy (many1 C.space) xml_attribute
	reservedOp "/>";
	return (EmptyXMLTag tag_name)


data XMLTag = XMLTag String deriving (Show)

xml_tag = do
	reservedOp "<";
	tag_name <- identifier
	attrs <- sepBy (many1 C.space) xml_attribute
	reservedOp ">";
	--trees <- content
	reservedOp "</"; C.string tag_name; reservedOp ">"
	return (XMLTag tag_name)

data XML =
	  EmptyTag EmptyXMLTag
	| NormalTag XMLTag
	deriving (Show)

xml_content =
	    do {tag <- try xml_empty_tag; return $ EmptyTag tag}
	<|> do {tag <- try xml_tag; return $ NormalTag tag}

data Content =
	  MustacheContent Mustache
	| XMLContent XML
	| TextContent Text
	deriving (Show)

content = do
	    do {mst <- trace "mustache" $ try mustache_content; return $ MustacheContent mst}
	<|> do {xml <- trace "xml" $ try xml_content; return $ XMLContent xml}
	<|> do {txt <- trace "text" $ try node_data; return $ TextContent txt}

template = many content

--mst_unescaped  ::= mst_open "{" mst_name "}" mst_close
--mst_section    ::= mst_open "#" mst_name     mst_close content* mst_open "/" mst_name mst_close
--mst_inverted   ::= mst_open "^" mst_name     mst_close content* mst_open "/" mst_name mst_close
--mst_section_s  ::= mst_open "#" mst_name     mst_close string*  mst_open "/" mst_name mst_close
--mst_inverted_s ::= mst_open "^" mst_name     mst_close string*  mst_open "/" mst_name mst_close
--mst_comment    ::= mst_open "!" mst_comment  mst_close
--mst_partial    ::= mst_open ">" mst_name     mst_close

{--
string = mst_s <|> text

xml_attr_value = many string

xml_attr =
	do {
		name  <- xml_attr_name
		value <- xml_attr_value
		return $ XMLAttr name value
	}




template :: Parser [Content]
template =
	do {
		trees <- many content;
		return trees
	}

content =
	do {
		xml <- xml;
		return xml
	} <|> do {
		mst <- mst;
		return mst
	} <|> do {
		text <- text
	}





produkt = do{ reserved "return"
            ; p <- price
            ; semi
            ; return (-p)
            }
      <|> do{ identifier
            ; p  <- price
            ; semi
            ; return p
            }
      <?> "produkt"

total   = do{ p <- price
            ; reserved "total"
            ; return p
            }
--}
