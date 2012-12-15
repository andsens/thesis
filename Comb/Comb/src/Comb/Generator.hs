{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Comb.Generator (
	generate
) where
import qualified Comb.Parser as P
import qualified Comb.Resolver as R
import Text.JSON.Types(toJSObject, toJSString, JSObject, JSValue(..))
import Text.Parsec.Pos(sourceLine)
import Data.Hashable(hash)
import Data.List
import Numeric
import Debug.Trace

jstring = JSString . toJSString
jobj = JSObject . toJSObject
jint = (JSRational True) . toRational

generate resolutions =
	toJSObject $ root_selector:(map make_selector resolutions)

root_selector =
	("root",
	jobj [
		("name", JSNull),
		("id", jstring "root"),
		("stack", JSArray []),
		("type", jstring "section"),
		("path", JSArray $ [jobj [("type", jstring "index"), ("i", jint 0)]]),
		("inverted", JSBool False),
		("prev", JSNull),
		("next", JSNull),
		("first", JSNull),
		("last", JSNull),
		("content_length", JSNull)
	])

make_selector r@(R.SectionSelector{..}) =
	(node_id node,
	jobj [
		("name", jstring $ P.name node),
		("id", jstring $ node_id node),
		("stack", JSArray $ (reverse . make_stack) parent_section),
		("type", jstring "section"),
		("path", JSArray $ (reverse . make_path) path),
		("inverted", JSBool $ P.inverted node),
		("prev", make_node previous_sibling),
		("next", make_node next_sibling),
		("first", make_node first_child),
		("last", make_node last_child),
		("content_length", jint content_length)
	])
make_selector r@(R.VariableSelector{..}) =
	(node_id node,
	jobj [
		("name", jstring $ P.name node),
		("id", jstring $ node_id node),
		("stack", JSArray $ (reverse . make_stack) parent_section),
		("type", jstring $ if is_escaped then "escaped" else "unescaped"),
		("path", JSArray $ (reverse . make_path) path),
		("prev", make_node previous_sibling),
		("next", make_node next_sibling)
	])
	where is_escaped = P.escaped node

make_path (R.Index i parent) =
	(jobj [("type", jstring "index"), ("i", jint i)]):(make_path parent)
make_path (R.Attribute name parent) =
	(jobj [("type", jstring "attribute"), ("name", jstring name)]):(make_path parent)
make_path (R.Offset i o) =
	[jobj [("type", jstring "offset"), ("i", jint i), ("node", jstring $ node_id (R.node o))]]
make_path (R.Child o) =
	[jobj [("type", jstring "child"), ("node", jstring $ node_id (R.node o))]]
make_path (R.Root i) =
	[jobj [("type", jstring "index"), ("i", jint i)], jobj [("type", jstring "child"), ("node", jstring "root")]]

node_id node =
	let nid = hash . show $ P.begin node
	in if nid >= 0 then showHex nid "1" else showHex (-nid) "0"

make_stack (Just R.SectionSelector{..}) = (jstring $ node_id node):(make_stack parent_section)
make_stack Nothing = [jstring "root"]

make_node (Just s@P.Section{..})   = jobj [("type", jstring "section"), ("id", jstring $ node_id s)]
make_node (Just v@P.Variable{escaped=True,..}) = jobj [("type", jstring "escaped"), ("id", jstring $ node_id v)]
make_node (Just v@P.Variable{escaped=False,..}) = jobj [("type", jstring "unescaped"), ("id", jstring $ node_id v)]
make_node (Just P.XMLTag{..})      = jobj [("type", jstring "node"), ("name", jstring name)]
make_node (Just P.EmptyXMLTag{..}) = jobj [("type", jstring "emptynode"), ("name", jstring name)]
make_node (Just P.XMLComment{..})  = jobj [("type", jstring "comment")]
make_node (Just P.Text{..})        = jobj [("type", jstring "text"), ("value", jstring text)]
make_node Nothing                  = jobj [("type", jstring "null")]



