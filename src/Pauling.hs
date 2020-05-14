module Pauling (isSatisfyP1, pauling2) where

import Type
import Basic
import Debug.Trace (trace)

naIon = Ion Plus Na 1 0.095
clIon = Ion Minus Cl 1 0.181

isSatisfyP1 :: Ion -> Ion -> Bool
isSatisfyP1 a b
    | pole a == Plus && pole b == Minus = pauling1 (radius a) (radius b)
    | pole a == Minus && pole b == Plus = pauling1 (radius b) (radius a)
    | otherwise = trace ("both of ion have same polarity" ++ show a ++ show b) False where
        pauling1 r1 r2 = r1 > r2*(sqrt 2 - 1)

--限界半径比から決まる最大配位数
maxCoordinateN :: Ion -> Ion -> Int
maxCoordinateN a b
    | pole a == Plus && pole b == Minus = coordinateN' (radius a) (radius b)
    | pole a == Minus && pole b == Plus = coordinateN' (radius b) (radius a)
    | otherwise = trace ("both of ion have same polarity" ++ show a ++ show b) 0

-- isSatisfyP2 n a b は，aからbに対する配位数と,bからaに対する配位数を返す
pauling2 :: Ion -> Ion -> (Coordinate, Coordinate)
pauling2 a b
    | pole a == Plus && pole b == Minus = (e_p2m,(eN b)*e_p2m `div` (eN a))
    | pole a == Minus && pole b == Plus = ((eN a)*e_p2m `div` (eN b), e_p2m )
    | otherwise = trace ("both of ion have same polarity" ++ show a ++ show b) (0,0) where
        e_p2m = maxCoordinateN a b

coordinateN' r1 r2
    | r1/r2 < 0.155 = 2
    | r1/r2 < 0.225 = 3
    | r1/r2 < 0.414 = 4
    | r1/r2 < 0.732 = 6 -- NaCl型
    | otherwise = 8     -- CsCl型