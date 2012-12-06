{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Comb.Generator (
) where
import qualified Comb.Parser as P
import qualified Comb.Resolver as R
import Text.Parsec.Pos(sourceLine)
import Data.List
import Debug.Trace

type Map = [Mapping]

generate :: R.Resolutions -> Map
generate r:rs = (make_selector res r):(generate rs)
generate [] = []

make_selector rs VariableSelector{..} =
	chain path


chain Index i parent = (chain parent) ++ ".childNodes[" ++ (show i) ++ "]"
chain Offset i o = "offset(\"" ++ (fq_name o) ++ "\", " ++ (show i) ++ ")"
chain Top i = "root.childNodes[" ++ (show i) ++ "]"

fq_name res = (scope $ parent_section res) ++ (P.name (node res))

scope Just (SectionSelector{parent_section=(Just parent),..}) = (P.name node) ++ "." ++ (scope parent)
scope Just (SectionSelector{parent_section=Nothing,..}) = P.name node
scope Nothing = ""
