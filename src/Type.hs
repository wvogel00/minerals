module Type where

type ValenceE = Int
type Coordinate = Int
type Radius = Float
type Distance = Float

data Pole = Plus | Minus deriving (Eq,Show)
data Ion = Ion {
    pole :: Pole,
    elem :: Element,
    eN :: ValenceE,
    radius :: Radius} deriving (Eq,Show)

data Mineral = Ionic [Ion] | Covalent [Element] | Vanderwaals [Element] deriving (Eq, Show)

--元素
data Element =
    H | He | Li | Be | B | C | N | O | F | Ne |
    Na | Mg | Al | Si | P | S | Cl | Ar |
    K | Ca | Sc | Ti | V | Cr | Mn | Fe | Co | Ni | Cu | Zn | Ga | Ge | As | Se | Br | Kr |
    Rb | Sr | Y | Zr | Nb | Mo | Tc | Ru | Rh | Pd | Ag | Cd | In | Sn | Sb | Te | I | Xe | 
    Cs | Ba | Hf | Ta | W | Re | Os | Ir | Pt | Au | Hg | Tl | Pb | Bi | Po | At | Rn |
    Fr | Ra | Ac | Th | Pa | U | Np | Pu | Am | Cm | Bk | Cf | Es | Fm | Md | No | Lr |
    La | Ce | Pr | Nd | Pm | Sm | Eu | Gd | Tb | Dy | Ho | Er | Tm | Yb | Lu
    deriving (Eq,Show,Enum,Ord)

data Structure = HCP | CCP | FCC | BCC | Some deriving (Eq)

-- 14種類のブラべ格子
-- P..単純 I..体心 F..面心 C..底心
data Bravais = PCube | ICube | FCube | PSquare | ISquare
            | PDiagonal | IDiagonal | FDiagonal | CDiagonal
            | PHexagonal | PTrigonal | RTrigonal
            | PMonoclinic | CMonoclinic | PTriclinic
            | ClosestHexagonal -- 最密六方格子
            deriving (Eq, Show)

--原子の配置は，頂点で共有 > 稜で共有 > 面で共有 の順に安定
data Polyhedron = Vertex | Ridge | Surface deriving (Eq, Show, Enum, Ord)

-- 岩塩型，閃亜鉛鉱型，ウルツ鉱型，塩化セシウム型
data Madelung = NaCl | ZnS | UZnS | CsCl deriving (Eq, Show)

data XML = XML{
    name :: Element, 
    structure :: Maybe Structure,
    _a :: Maybe Float,
    _c :: Maybe Float,
    _density :: Maybe Float,
    _ions :: [Ion]
    }
    deriving (Eq,Show)


instance Show Structure where
    show HCP = "hexagonal close-packed structure"
    show CCP = "cubic close-packed structure"
    show FCC = "facxe-centered cubic structure"
    show BCC = "body-centered cubic structure"
    show Some = "any kind of structures"