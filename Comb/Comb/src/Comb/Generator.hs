{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Comb.Generator (
	generate
) where
import qualified Comb.Parser as P
import qualified Comb.Resolver as R
import Text.JSON.Types(toJSObject, toJSString, JSObject, JSValue(..))
import Text.Parsec.Pos(sourceLine)
import Data.Hashable(hash)
import Numeric
import Data.Maybe(listToMaybe)
import Debug.Trace

jstring = JSString . toJSString
jobj = JSObject . toJSObject
jint = (JSRational True) . toRational

generate resolutions =
	toJSObject $ root_selector:(map make_selector resolutions)

root_selector =
	("root",
	jobj [
		("name", jstring "root"),
		("id", jstring "root"),
		("stack", JSArray []),
		("type", jstring "section"),
		("path", JSArray $ [jobj [("type", jstring "index"), ("i", jint 0)]]),
		("inverted", JSBool False),
		("prev", jobj [("type", jstring "null")]),
		("next", jobj [("type", jstring "null")]),
		("first", JSNull),
		("last", JSNull),
		("contents", JSArray $ [])
	])

make_selector r@(R.SectionSelector{..}) =
	(node_id node,
	jobj [
		("name", jstring $ P.name node),
		("id", jstring $ node_id node),
		("stack", JSArray $ (reverse . make_stack) section),
		("type", jstring "section"),
		("path", JSArray $ (reverse . make_path) path),
		("inverted", JSBool $ P.inverted node),
		("prev", make_node prev),
		("next", make_node next),
		("first", make_node first_c),
		("last", make_node last_c),
		("contents", JSArray $ make_content contents)
	])
	where contents = P.contents node
make_selector r@(R.VariableSelector{..}) =
	(node_id node,
	jobj [
		("name", jstring $ P.name node),
		("id", jstring $ node_id node),
		("stack", JSArray $ (reverse . make_stack) section),
		("type", jstring $ if is_escaped then "escaped" else "unescaped"),
		("path", JSArray $ (reverse . make_path) path),
		("prev", make_node prev),
		("next", make_node next)
	])
	where is_escaped = P.escaped node

make_path (R.Index i parent) =
	(jobj [("type", jstring "index"), ("i", jint i)]):(make_path parent)
make_path (R.Attribute name parent) =
	(jobj [("type", jstring "attribute"), ("name", jstring name)]):(make_path parent)
make_path (R.Offset o) =
	[jobj [("type", jstring "offset"), ("node", jstring $ node_id (R.node o))]]
make_path (R.Child o) =
	[jobj [("type", jstring "child"), ("node", jstring $ node_id (R.node o))]]
make_path R.Root =
	[jobj [("type", jstring "child"), ("node", jstring "root")]]

node_id node =
	let nid = hash . show $ P.begin node
	in if nid >= 0 then showHex nid "1" else showHex (-nid) "0"

make_stack (Just R.SectionSelector{..}) = (jstring $ node_id node):(make_stack section)
make_stack Nothing = [jstring "root"]

make_node (Just s@P.Section{})     = jobj [("type", jstring "section"), ("id", jstring $ node_id s)]
make_node (Just v@P.Variable{escaped=True,..})  = jobj [("type", jstring "escaped"), ("id", jstring $ node_id v)]
make_node (Just v@P.Variable{escaped=False,..}) = jobj [("type", jstring "unescaped"), ("id", jstring $ node_id v)]
make_node (Just P.XMLTag{..})      = jobj [("type", jstring "node"), ("name", jstring name)]
make_node (Just P.EmptyXMLTag{..}) = jobj [("type", jstring "emptynode"), ("name", jstring name)]
make_node (Just P.XMLComment{..})  = jobj [("type", jstring "comment")]
make_node (Just P.Text{..})        = jobj [("type", jstring "text"), ("value", jstring text)]
make_node Nothing                  = jobj [("type", jstring "null")]


make_content (s@P.Section{}:content)   = (jstring $ node_id s):(make_content content)
make_content (v@P.Variable{}:content)  = (jstring $ node_id v):(make_content content)
make_content (P.XMLTag{}:content)      = (jstring "#node"):(make_content content)
make_content (P.EmptyXMLTag{}:content) = (jstring "#emptynode"):(make_content content)
make_content (P.XMLComment{}:content)  = (jstring "#comment"):(make_content content)
make_content (P.Text{}:content)        = (jstring "#text"):(make_content content)
make_content []                        = []

