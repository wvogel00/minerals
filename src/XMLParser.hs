module XMLParser where
-- parser for XML of elements

import Text.Trifecta
import Text.Trifecta.Delta (Delta(..))
import Data.List
import Data.Maybe
import Control.Applicative
import Basic
import Type

runParseXML :: String -> Result [XML]
runParseXML = parseString parseXML startPos

startPos = Columns 0 0

parseXML :: Parser [XML]
parseXML = do
    symbol "<elements>"
    spaces
    result <- (:) <$> xml <*> many xml
    spaces
    symbol "</elements>"
    return result

wholetest = "<element>\n <name>H</name> <structure>hcp</structure> <a>0.375</a> <c>0.612</c> <density>0.076</density>"
    ++ test2 ++ "</element>"

test = " <element> <name>At</name>    </element><element><name>Rn</name></element>"
test2 = "<ion> <e>-1</e> <radius>0.14</radius>  </ion>"
xml :: Parser XML
xml = do
    spaces >> symbol "<element>"
    n <- (spaces *> elemName <* spaces)
    st <- try (spaces *> elemStructure <* spaces) <|> return Nothing
    a <- try (spaces *> elemA <* spaces) <|> return Nothing
    c <- try (spaces *> elemC <* spaces) <|> return Nothing
    d <- try (spaces *> elemDensity <* spaces) <|> return Nothing
    is <- try (spaces *> (elemIons n) <* spaces) <|> return []
    symbol "</element>" >> spaces
    return XML{name=n, structure=st, _a=a, _c=c, _density=d, _ions=is}

elemName = fromJust.findElem <$> (symbol "<name>" *> many alphaNum <* symbol "</name>")

elemStructure :: Parser (Maybe Structure)
elemStructure = classify <$> (symbol "<structure>" *> many alphaNum <* symbol "</structure>") where
    classify str = case str of
        "hcp" -> Just HCP
        "bcc" -> Just BCC
        "ccp" -> Just CCP
        "fcc" -> Just FCC
        "many" -> Just Some
        _ -> Nothing

floatN = (\a b c -> f (a++[b]++c)) <$> many digit <*> try dot <*> try (many digit) where
    f [] = Nothing
    f x = Just (read x)

elemA, elemC, elemDensity :: Parser (Maybe Float)
elemA = symbol "<a>" *> floatN <* symbol "</a>" 
elemC = symbol "<c>" *> floatN <* symbol "</c>" 
elemDensity = symbol "<density>" *> floatN <* symbol "</density>" 

elemIons :: Element -> Parser [Ion]
elemIons n = many (ion n)

ion n = do
    spaces
    try $ symbol "<ion>"
    (sign, e) <- try (spaces *> ionE <* spaces)
    r <- try (spaces *> ionR <* spaces)
    try $ symbol "</ion>"
    spaces
    return Ion{pole=sign, Type.elem=n, eN=e, radius=fromJust r}

ionE :: Parser (Pole, ValenceE)
ionE = (\s e -> (getPole s, read e)) <$> (symbol "<e>" *> oneOf "+-") <*> (many digit <* symbol "</e>")
ionR = symbol "<radius>" *> floatN <* symbol "</radius>"

getPole '-' = Minus
getPole _ = Plus

findElem :: String -> Maybe Element
findElem elem = find((==elem).show) [H ..]