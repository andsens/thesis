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

generate resolutions =
	toJSObject $ map make_selector resolutions

jstring = JSString . toJSString
jobj = JSObject . toJSObject
jint = (JSRational True) . toRational

make_selector r@(R.SectionSelector{..}) =
	(node_id node,
	jobj [
		("name", jstring $ P.name node),
		("stack", JSArray $ make_stack parent_section),
		("type", jstring "section"),
		("path", JSArray $ make_path path),
		("inverted", JSBool $ P.inverted node),
		("prev", make_node previous_sibling),
		("next", make_node next_sibling),
		("first", make_node first_child),
		("last", make_node last_child)
	])
make_selector r@(R.VariableSelector{..}) =
	(node_id node,
	jobj [
		("name", jstring $ R.fq_name r),
		("stack", JSArray $ make_stack parent_section),
		("type", jstring "variable"),
		("path", JSArray $ make_path path),
		("escaped", JSBool $ P.escaped node),
		("prev", make_node previous_sibling),
		("next", make_node next_sibling)
	])

make_path (R.Index i parent) =
	(jobj [("type", jstring "index"), ("i", jint i)]):(make_path parent)
make_path (R.Attribute name parent) =
	(jobj [("type", jstring "attribute"), ("name", jstring name)]):(make_path parent)
make_path (R.Offset o) =
	[jobj [("type", jstring "offset"), ("node", jstring $ node_id (R.node o))]]
make_path R.Root =
	[jobj [("type", jstring "root")]]

--node_id :: P.Content -> ShowS
node_id node =
	let nid = hash . show $ P.begin node
	in if nid >= 0 then showHex nid "1" else showHex (-nid) "0"

make_stack (Just R.SectionSelector{..}) = (jstring $ P.name node):(make_stack parent_section)
make_stack Nothing = []

make_node (Just s@P.Section{..})   = jobj [("type", jstring "section"), ("id", jstring $ node_id s)]
make_node (Just v@P.Variable{..})  = jobj [("type", jstring "variable"), ("id", jstring $ node_id v)]
make_node (Just P.XMLTag{..})      = jobj [("type", jstring "node"), ("name", jstring name)]
make_node (Just P.EmptyXMLTag{..}) = jobj [("type", jstring "emptynode"), ("name", jstring name)]
make_node (Just P.XMLComment{..})  = jobj [("type", jstring "comment")]
make_node (Just P.Text{..})        = jobj [("type", jstring "text"), ("value", jstring text)]
make_node Nothing                  = JSNull



