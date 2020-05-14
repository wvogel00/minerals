module CFParser(runParseCF) where
-- parser for chemical formula

import Text.Trifecta
import Text.Trifecta.Delta (Delta(..))
import Data.List
import Data.Maybe
import Control.Applicative
import Type

runParseCF :: String -> Result [(Maybe Element, Int)]
runParseCF = parseString parseCF startPos

startPos = Columns 0 0

parseCF :: Parser [(Maybe Element, Int)]
parseCF = (:) <$> element <*> many element

element :: Parser (Maybe Element, Int)
element = (tuple <$> upper <*> try (many lower) <*> try (many digit)) where
    tuple u l n = (findElem $ u:l, safeRead n)
    safeRead "" = 1
    safeRead x = read x

findElem :: String -> Maybe Element
findElem elem = find((==elem).show) [H ..]