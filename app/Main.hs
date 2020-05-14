{-# LANGUAGE BangPatterns #-}
module Main where

import Vis
import Linear.V3
import Mineral
import Pauling
import Type
import CFParser
import XMLParser
import Basic (testModel, fromSuccess, drawFormula)
import System.Environment (getArgs)
import Data.Maybe
import Data.List
import System.Directory
import Debug.Trace

mineralWindow = defaultOpts{
    optWindowName = "Mineral Simulator",
    optWindowSize = Just (640,360),
    optWindowPosition = Just (500,100),
    optBackgroundColor = Just white
}


main :: IO ()
main = do
    (formula:_) <- getArgs
    dbFilepath <- (\p -> p++"/db/elements.xml") <$> getCurrentDirectory
    xmlfile <- readFile dbFilepath
    let !xmls = fromSuccess $ runParseXML xmlfile
    if formula == []
        then putStrLn "結晶の化学式を入力してください"
        else draw formula xmls

draw :: String -> [XML] -> IO ()
draw formula xmls = do
    let elems = fromSuccess $ runParseCF formula
    let objects = (case length elems of
                1 -> monoCrystal.fromJust $ find (\xml -> name xml == (fromJust $ fst $ head elems)) xmls
                2 -> polyCrystal elems xmls
                _ -> []
                )
    display mineralWindow $ VisObjects $ drawFormula formula : objects  -- testModel