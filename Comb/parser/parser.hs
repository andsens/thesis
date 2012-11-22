module Mustache where
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T
import qualified Text.ParserCombinators.Parsec.Char as C
import Text.ParserCombinators.Parsec.Language( emptyDef )

run :: Show a => Parser a -> String -> IO ()
run p input = case (parse p "" input) of
	Left err -> do {
		putStr "parse error at ";
		print err
	}
	Right x  -> print x

lexer :: T.TokenParser ()
lexer  = T.makeTokenParser (
	emptyDef {
		T.reservedOpNames = ["<", "</", "{{", "}}", "/>", ">"],
		T.caseSensitive = False
	})

identifier = T.identifier lexer
symbol     = T.symbol lexer
reservedOp = T.reservedOp lexer

x_start_ops = ["<", "</"]
x_end_ops   = [">", "/>"]
m_start_ops = ["{{", "{{{", "{{#", "{{^", "{{!", "{{>"]
m_end_ops   = ["}}", "}}}"]

data NodeData = Data String

nodeData = 
	do
		str <- try (manyTill anyChar $ choice . map (try . reservedOp) $ (x_start_ops ++ m_start_ops))
		return $ Data str
	<|>
	do
		str <- many anyChar
		return $ Data str


data GenVariable =
	  Escaped String
	| Unescaped String
	| Comment String
	| Partial String
	deriving (Show)

m_escaped   = do { reservedOp "{{"; var_name <- identifier; reservedOp "}}"; return $ Escaped var_name }
m_unescaped = do { reservedOp "{{{"; var_name <- identifier; reservedOp "}}}"; return $ Unescaped var_name }


data GenSection =
	  NormalSection String
	| InvertedSection String
	deriving (Show)

m_content_section = do
	reservedOp "{{#"; var_name <- identifier; reservedOp "}}"
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

mustache_content :: GenParser Char () Mustache
mustache_content =
	    do {section <- try m_content_section; return $ Section section}
	<|> do {section <- try m_content_inverted; return $ Section section}
	<|> do {variable <- try m_escaped; return $ Variable variable}
	<|> do {variable <- try m_unescaped; return $ Variable variable}


data EmptyXMLTag = EmptyXMLTag String

xml_empty_tag = do
	reservedOp "<";
	tag_name <- identifier
	--attrs <- xml_attributes
	reservedOp "/>";
	return (EmptyXMLTag tag_name)


data XMLTag = XMLTag String

xml_tag = do
	reservedOp "<";
	tag_name <- identifier
	--attrs <- xml_attributes
	reservedOp ">";
	--trees <- content
	reservedOp "</"; C.string tag_name; reservedOp ">"
	return (XMLTag tag_name)

data XML =
	  EmptyTag EmptyXMLTag
	| NormalTag XMLTag

xml_content :: GenParser Char () XML
xml_content =
	    do {tag <- try xml_empty_tag; return $ EmptyTag tag}
	<|> do {tag <- try xml_tag; return $ NormalTag tag}



--content = do
--	try must

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
