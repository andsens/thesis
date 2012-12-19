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
import Data.List(elemIndex,findIndex)

jstring = JSString . toJSString
jobj = JSObject . toJSObject
jint = (JSRational True) . toRational

generate resolutions =
	root_selector:(map (make_selector resolutions) resolutions)

root_selector =
	jobj [
		("name", jstring "root"),
		("section", JSNull),
		("type", jstring "section"),
		("offset", JSBool False),
		("path", JSArray $ [jint 0]),
		("inverted", JSBool False),
		("prev", jobj [("type", jstring "null")]),
		("next", jobj [("type", jstring "null")]),
		("first", JSNull),
		("last", JSNull),
		("contents", JSArray $ [])]

make_selector resolutions r@(R.SectionSelector{..}) =
	jobj [
		("name", jstring $ P.name node),
		("section", case section of Just section -> index section resolutions; Nothing -> jint 0),
		("type", jstring "section"),
		("offset", is_offset path),
		("path", JSArray $ (reverse . (make_path resolutions)) path),
		("inverted", JSBool $ P.inverted node),
		("prev",  make_node resolutions prev),
		("next",  make_node resolutions next),
		("first", make_node resolutions first_c),
		("last",  make_node resolutions last_c),
		("contents", JSArray $ map (make_content resolutions) (P.contents node))]
make_selector resolutions r@(R.PartialSelector{..}) =
	jobj [
		("name", jstring $ P.name node),
		("section", case section of Just section -> index section resolutions; Nothing -> jint 0),
		("type", jstring "partial"),
		("offset", is_offset path),
		("path", JSArray $ (reverse . (make_path resolutions)) path),
		("prev", make_node resolutions prev),
		("next", make_node resolutions next)]
make_selector resolutions r@(R.VariableSelector{..}) =
	jobj [
		("name", jstring $ P.name node),
		("section", case section of Just section -> index section resolutions; Nothing -> jint 0),
		("type", jstring $ if (P.escaped node) then "escaped" else "unescaped"),
		("offset", is_offset path),
		("path", JSArray $ (reverse . (make_path resolutions)) path),
		("prev", make_node resolutions prev),
		("next", make_node resolutions next)]

is_offset (R.Index _ parent) = is_offset parent
is_offset (R.Attribute _ parent) = is_offset parent
is_offset R.Offset{} = JSBool True
is_offset R.Child{} = JSBool False
is_offset R.Root{} = JSBool False

make_path resolutions (R.Index i parent) = (jint i):(make_path resolutions parent)
make_path resolutions (R.Attribute name parent) = (jstring name):(make_path resolutions parent)
make_path resolutions (R.Offset o) = [index o resolutions]
make_path resolutions (R.Child o) = [index o resolutions]
make_path resolutions (R.Root) = [jint 0]

make_node resolutions (Just s@P.Section{})   = jobj [("type", jstring "section"), ("id", node_index s resolutions)]
make_node resolutions (Just p@P.Partial{..}) = jobj [("type", jstring "partial"), ("id", node_index p resolutions)]
make_node resolutions (Just v@P.Variable{escaped=True,..})  = jobj [("type", jstring "escaped"), ("id", node_index v resolutions)]
make_node resolutions (Just v@P.Variable{escaped=False,..}) = jobj [("type", jstring "unescaped"), ("id", node_index v resolutions)]
make_node _ (Just P.XMLTag{..})      = jobj [("type", jstring "node"), ("name", jstring name)]
make_node _ (Just P.EmptyXMLTag{..}) = jobj [("type", jstring "emptynode"), ("name", jstring name)]
make_node _ (Just P.XMLComment{..})  = jobj [("type", jstring "comment")]
make_node _ (Just P.Text{..})        = jobj [("type", jstring "text"), ("value", jstring text)]
make_node _ Nothing                  = jobj [("type", jstring "null")]

make_content resolutions s@P.Section{}  = (node_index s resolutions)
make_content resolutions v@P.Partial{}  = (node_index v resolutions)
make_content resolutions v@P.Variable{} = (node_index v resolutions)
make_content _ P.XMLTag{}               = (jstring "#node")
make_content _ P.EmptyXMLTag{}          = (jstring "#emptynode")
make_content _ P.XMLComment{}           = (jstring "#comment")
make_content _ P.Text{}                 = (jstring "#text")

index :: R.Resolution -> R.Resolutions -> JSValue
index res resolutions = case elemIndex res resolutions of
	Just i -> jint (i+1) -- We prepend the root node
	Nothing -> error "Resolution index not found"

node_index :: P.Content -> R.Resolutions -> JSValue
node_index node resolutions = case findIndex (\x -> R.node x == node) resolutions of
	Just i -> jint (i+1) -- We prepend the root node
	Nothing -> error ("Resolution index not found for node "++show node++" not found")
