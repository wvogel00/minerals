module Mineral where

import Vis
import Linear.V3
import Pauling
import Type
import CFParser
import XMLParser
import Basic
import Data.Maybe (Maybe(..), fromJust)
import Data.List (find)
import SpatialMath (Euler (..))

na = Ion{Type.elem = Na, eN = 1, pole = Plus, radius = 0.15}
cl = Ion{Type.elem = Cl, eN = negate 1, pole = Minus, radius = 0.3}

monoCrystal :: XML -> [VisObject Float]
monoCrystal xml = case structure xml of
    Just HCP -> hcpMonoStructure (fromJust $ _a xml) (fromJust $ _c xml)
    Just CCP -> ccpMonoStructure (fromJust $ _a xml)
    Just FCC -> ccpMonoStructure (fromJust $ _a xml)
    Nothing -> [drawText (show (name xml) ++" have no mono-crystal")]

--六方最密格子
hcpMonoStructure :: Float -> Float -> [VisObject Float]
hcpMonoStructure a c = bones ++ elems where
    vertexs = map (rot2D (V3 (-a/2) (a/2*sqrt 3) 0)) [0,60..360] :: [V3 Float]
    (top,bottom) = (map (movPoint (0,0,c)) vertexs, map (movPoint (0,0,-c)) vertexs)
    center = map (rot2D (V3 (a/sqrt 3) 0 0)) [30,150,270]
    bones = [toBone top, toBone bottom] ++ ( map (toBone.(\tuple -> [fst tuple,  snd tuple])) $ zip top bottom )
    elems = map applyMove $ zip (top++bottom++center) $ repeat (Sphere (a/3) Solid yellow)
    
-- 立方最密格子
ccpMonoStructure a = bones ++ (map applyMove.zip poses $ repeat (Sphere (a/3) Solid yellow)) where
    (bones,poses) = bravais FCube (a,a,a) (90,90,90)

polyCrystal formula xmls = case getStructure xmls of
    Ionic ions -> genIonStructure CCP ions
    Covalent elems -> []
    Vanderwaals elems -> []

getStructure _ = Ionic [na,cl]

genIonStructure :: Structure -> [Ion] -> [VisObject Float]
genIonStructure HCP = hcpStructure
genIonStructure CCP = ccpStructure
genIonStructure FCC = ccpStructure -- have the same structre as ccp
genIonStructure BCC = bccStructure

hcpStructure _ = []
bccStructure _ = []

ccpStructure :: [Ion] -> [VisObject Float]
ccpStructure (a:b:_) = bones : ions where
    ions = map (moveZ (edgeLen/2)) (ionOnVertexs b a) ++ ionOnVertexs a b ++ map (moveZ (negate edgeLen/2)) (ionOnVertexs b a)
    edgeLen = 1.0
    bones = Cube edgeLen Wireframe blue
    obj c = Sphere (radius c) Solid (genColor c)
    ionOnVertexs c d = map applyMove.zip ionPoses $ concat $ repeat [obj c,obj d]
    ionPoses = [(V3 x y 0) | x <- [-edgeLen/2,0,edgeLen/2], y <- [-edgeLen/2,0,edgeLen/2]]

applyMove (v3,v) = Trans v3 v
rot2D (V3 x y z) theta = V3 (x*cos rad - y*sin rad) (x*sin rad + y * cos rad) z where
    rad = theta/360*2*pi
movPoint (mx,my,mz) (V3 x y z) = V3 (x+mx) (y+my) (z+mz)

toBone ls = Line (Just 0.1) ls blue
rotZ z = RotEulerDeg (Euler z 0 0)
moveX x = Trans (V3 x 0 0)
moveY y = Trans (V3 0 y 0)
moveZ z = Trans (V3 0 0 z)
moveXY x y = Trans (V3 x y 0)

genColor c = if pole c == Plus then blue else yellow

--------- 14 kinds of Bravais Grid ------------
type WireObject = [VisObject Float]

bravais :: Bravais -> (Float, Float, Float) -> (Float,Float,Float) -> (WireObject, [V3 Float])

-- 立方格子
bravais PCube (a,_,_ ) _ = bravais PTriclinic (a,a,a) (90,90,90)

bravais ICube (a,_,_) _ = ([Cube (2*a) Wireframe blue] ++ wires, V3 0 0 0 : [V3 x y z | x <- [-a,a], y <- [-a,a], z <- [-a,a]]) where
    wires = map toBone $ map (\(x,y) -> [V3 x y a, V3 (-x) (-y) (-a)]) [(x,y) | x <- [-a,a], y <- [-a,a]]

bravais FCube (a,_,_) degs = (pcube++wire, pposes ++ fposes) where
    wire = concat $ map (\(V3 x y z) -> map toBone [[V3 x y z, V3 x (-y) (-z)],[V3 x y z, V3 (-x) y (-z)],[V3 x y z, V3 (-x) (-y) z]]) pposes
    fposes = pposes ++ [V3 x y z | x <- [-a,0,a], y <- [-a,0,a], z <- [-a,0,a], x==y && y/=z && y==0 || y==z&&z/=x && z==0 || z==x && x/=y && x==0]
    (pcube,pposes) = bravais PCube (a,a,a) degs

-- 正方格子
bravais PSquare (a,_, c) _ = bravais PTriclinic (a,a,c) (90,90,90)

bravais ISquare (a,_, c) degs = (wire++pwire, V3 0 0 0 : pposes) where
    wire = map toBone $ map (\(x,y) -> [V3 x y c, V3 (-x) (-y) (-c)]) [(x,y) | x <- [-a,a], y <- [-a,a]]
    (pwire, pposes) = bravais PSquare (a,a,c) degs

-- 斜方格子(Diagonal grid)
bravais PDiagonal abc _ = bravais PTriclinic abc (90,90,90)

bravais IDiagonal (a, b, c) degs = (wire++pwire, V3 0 0 0 : pposes) where
    wire = map toBone $ map (\(x,y) -> [V3 x y c, V3 (-x) (-y) (-c)]) [(x,y) | x <- [-a,a], y <- [-b,b]]
    (pwire, pposes) = bravais PDiagonal (a,b, c) degs

bravais FDiagonal (a, b, c) degs = (pcube++wire, pposes ++ fposes) where
    wire = concat $ map (\(V3 x y z) -> map toBone [[V3 x y z, V3 x (-y) (-z)],[V3 x y z, V3 (-x) y (-z)],[V3 x y z, V3 (-x) (-y) z]]) pposes
    fposes = pposes ++ [V3 x y z | x <- [-a,0,a], y <- [-b,0,b], z <- [-c,0,c], x==0 && y==0 && z/=0 || x/=0 && y==0 && z==0 || x==0 && y/=0 && z==0]
    (pcube,pposes) = bravais PDiagonal (a,b,c) degs

bravais CDiagonal (a, b, c) degs = (pcube++wire,V3 0 0 c : V3 0 0 (-c) : pposes) where
    wire = map toBone $ map (\(x,z) -> [V3 x b z, V3 (-x) (-b) z])[(x,z) | x <- [-a,a], z <- [-c,c]]
    (pcube,pposes) = bravais PDiagonal (a,b,c) degs

-- 単純六方晶系
bravais PHexagonal (a,_, c) _ = (wires, poses) where
    wires = vertBone ++ hexbone ++  hexwires ++ map (moveZ (-2*c)) (hexbone ++ hexwires)
    vertBone = map (\deg -> rotZ deg (toBone [V3 (a/2) (sqrt 3/2*a) c, V3 (a/2) (sqrt 3/2*a) (-c)] )) [0,60..300]
    hexbone = map (\deg -> rotZ deg (toBone [V3 (a/2) (sqrt 3/2*a) c, V3 (-a/2) (sqrt 3/2*a) c] )) [0,60..300]
    hexwires = map (\deg -> rotZ deg (toBone [V3 0 0 c, V3 (-a/2) (sqrt 3/2*a) c] )) [0,120,240]
    poses = V3 0 0 c:V3 0 0 (-c):map (rot2D (V3 (a/2) (sqrt 3/2*a) c)) [0,60..300] ++ map (rot2D (V3 (-a/2) (sqrt 3/2*a) (-c))) [0,60..300]

bravais PTrigonal (a,_, c) _ = (wires, poses) where
    wires = vertBone ++ tribone ++ map (moveZ (-2*c)) tribone
    vertBone = toBone [V3 0 0 c, V3 0 0 (-c)] : map (\deg -> rotZ deg (toBone [V3 (-a/2) (sqrt 3/2*a) c, V3 (-a/2) (sqrt 3/2*a) (-c)] )) [0,60,120]
    tribone = [toBone (V3 0 0 c : vecs)]
    poses = vecs ++ map (\(V3 x y z) -> V3 x y (-z)) vecs
    vecs = [V3 (-a/2) (sqrt 3/2*a) c, V3 (-a) 0 c, V3 (-a/2) (-sqrt 3/2*a) c, V3 0 0 c ]

-- 斜方晶系は alpha==beta==gamma/=90deg
bravais RTrigonal (a,_,_) (alpha,beta,gamma) = if alpha == beta || beta == gamma then undefined else ([],[])

-- 単斜晶系
bravais PMonoclinic abc (_,beta,_) = bravais PTriclinic abc (90,beta,90)

bravais CMonoclinic ls@(_,_, c) ds@(_,beta,_) = (tops++bottoms++pwire, poses++pposes) where
    tops = map (\(V3 x y z) -> toBone [V3 x y z, V3 (-x) (-y) z]) $ filter (\(V3 x y z) -> x > 0 && z > 0) pposes
    bottoms = map (\(V3 x y z) -> toBone [V3 x y z, V3 (-x+2*c*cos beta) (-y) z]) $ filter (\(V3 x y z) -> x > 0 && z < 0) pposes
    poses = [V3 0 0 c, V3 (c*cos beta) 0 (-c)] 
    (pwire, pposes) = bravais PMonoclinic ls ds

bravais PTriclinic (a, b, c) (alpha, beta, gamma) = (wires, tops++bottoms) where
    wires = map toBone [tops, bottoms] ++ vertwires
    tops = [V3 (-a*sin gamma) (-b) c, V3 (-a*sin gamma) b c, V3 (a*sin gamma) (b+2*a*cos gamma) c, V3 (a*sin gamma) (-b+2*a*cos gamma) c, V3 (-a*sin gamma) (-b) c]
    bottoms = map (\(V3 x y z) -> V3 (x+c*cos beta) (y+c*sin alpha) (z-2*c)) tops
    vertwires = map (toBone.(\(v1,v2) -> [v1,v2])) $ zip tops bottoms

invert (V3 a b c) = V3 (-a) (-b) (-c)
(-.) = invert


