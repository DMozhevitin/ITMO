module Task8 where

import Control.Monad (liftM2)
import System.Random (StdGen, split, mkStdGen, randomR)
import Data.List (intercalate)
import Control.Monad (forM_)

class Functor w => Comonad w where
    extract   :: w a -> a
    duplicate :: w a -> w (w a)          
    extend    :: (w a -> b) -> w a -> w b
    extend f = fmap f <$> duplicate

data ListZipper a = LZ [a] a [a]

instance (Show a) => Show (ListZipper a) where
  show (LZ l x r) = "║" ++ (concatMap show l') ++ (show x) ++ (concatMap show r') ++ "║"  where
    l' = reverse $ take gridRadius l
    r' = take gridRadius r

gridRadius :: Int
gridRadius = 5

subList :: ListZipper a -> [a]
subList (LZ l x r) = l' ++ [x] ++ r' where
  l' = reverse $ take gridRadius l
  r' = take gridRadius r

listLeft, listRight :: ListZipper a -> ListZipper a
listLeft  (LZ (a:as) x bs) = LZ as a (x:bs)
listLeft _ = error "listLeft"

listRight (LZ as x (b:bs)) = LZ (x:as) b bs
listRight _ = error "listRight"

listWrite :: a -> ListZipper a -> ListZipper a
listWrite x (LZ ls _ rs) = LZ ls x rs

toList :: ListZipper a -> Int -> [a]
toList (LZ ls x rs) n = reverse (take n ls) ++ [x] ++ take n rs

instance Functor ListZipper where
    fmap f (LZ ls x rs) = LZ (map f ls) (f x) (map f rs)

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

mkZipper' :: (v -> v) -> (v -> v) -> v -> ListZipper v
mkZipper' genLeft genRight e = LZ (iterateTail genLeft e) e (iterateTail genRight e)

instance Comonad ListZipper where
    extract (LZ _ x _) = x
    duplicate = mkZipper' listLeft listRight

newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

instance (Show a) => Show (Grid a) where
  show (Grid g) = intercalate "\n" gridWithBorder where
    topBorder = "╔" ++ (replicate (2 * gridRadius + 1) '═') ++ "╗"
    bottomBorder = "╚" ++ (replicate (2 * gridRadius + 1) '═') ++ "╝"

    gridStr = fmap show (subList g)
    gridWithBorder = [topBorder] ++ gridStr ++ [bottomBorder]

instance Functor Grid where
    fmap f (Grid g) = Grid $ fmap f <$> g

up, down :: Grid a -> Grid a
up   (Grid g) = Grid (listLeft  g)
down (Grid g) = Grid (listRight g)  

left, right :: Grid a -> Grid a
left  (Grid g) = Grid (fmap listLeft  g)
right (Grid g) = Grid (fmap listRight g)

gridRead :: Grid a -> a
gridRead (Grid g) = extract $ extract g
 
gridWrite :: a -> Grid a -> Grid a
gridWrite x (Grid g) = Grid $ listWrite newLine g
  where
    oldLine = extract g
    newLine = listWrite x oldLine

horizontal, vertical :: Grid a -> ListZipper (Grid a)
horizontal = mkZipper' left right
vertical   = mkZipper' up   down

instance Comonad Grid where
    extract = gridRead
    duplicate = Grid . fmap horizontal . vertical

data Cell
  = Cell
  { gen :: StdGen
  , health :: HealthStatus  
  }

instance Show Cell where
  show (Cell _ (Healthy 0)) = " "
  show (Cell _ (Healthy _)) = "@"
  show (Cell _ (Infected _)) = "#"
  show (Cell _ (Ill _)) = "#" 

data HealthStatus
  = Healthy Int
  | Infected Int
  | Ill Int


data Cfg
  = Cfg
  { prob :: Double
  , incubationPeriod :: Int
  , illnessDuration :: Int
  , immunityDuration :: Int
  }

oneDayTick :: HealthStatus -> HealthStatus
oneDayTick (Healthy x) = Healthy $ pred x
oneDayTick (Infected x) = Infected $ pred x
oneDayTick (Ill x) = Ill $ pred x
                  
initialGrid :: Cfg -> Grid Cell
initialGrid cfg = gridWrite startCell g where
  g = emptyGrid
  startCell' = extract g
  startCell = startCell' { health = Infected $ incubationPeriod cfg}  

emptyGrid :: Grid Cell
emptyGrid = Grid $ mkZipper' mkLeftZipper mkRightZipper emptyRow

emptyRow :: ListZipper Cell
emptyRow = mkZipper' mkLeftCell mkRightCell healthyCell where
  healthyCell = Cell { gen = mkStdGen 229, health = Healthy 0}

mkLeftCell :: Cell -> Cell
mkLeftCell = mkNeighboringCell fst

mkLeftZipper :: ListZipper Cell -> ListZipper Cell
mkLeftZipper = fmap mkLeftCell

mkRightCell :: Cell -> Cell
mkRightCell = mkNeighboringCell snd

mkRightZipper :: ListZipper Cell -> ListZipper Cell
mkRightZipper = fmap mkRightCell

mkNeighboringCell :: ((StdGen, StdGen) -> StdGen) -> Cell -> Cell
mkNeighboringCell choose c = c { gen = newGen } where
  newGen = choose $ split $ gen c

maybeInfectCell :: Cfg -> Grid Cell -> Cell
maybeInfectCell cfg grid = cell' where
  cnt = dangerousCount grid
  curCell = extract grid
  g = gen curCell 
  b = rollDice cnt (prob cfg) g
  cell' = if b then
    curCell { health = Infected $ incubationPeriod cfg } 
  else 
    curCell

  rollDice :: Int -> Double -> StdGen -> Bool
  rollDice 0 _ _ = False
  rollDice cnt' prob' g' = if (p <= prob') then
      True
    else
      rollDice (pred cnt') prob' g''
      where
        (p, g'') = randomR (0.0, 1.0) g'
      
isDangerous :: Cell -> Bool
isDangerous Cell { health = (Infected _) } = True
isDangerous Cell { health = (Ill _) } = True
isDangerous _                              = False

neighbours :: [Grid a -> Grid a]
neighbours = horizontals ++ verticals ++ liftM2 (.) horizontals verticals
  where horizontals = [left, right]
        verticals   = [up, down]

dangerousCount :: Grid Cell -> Int
dangerousCount g = dangerousCount'
                  $ map (\direction -> extract $ direction g) neighbours
                  where
                    dangerousCount' = length . filter isDangerous

rule :: Cfg -> Grid Cell -> Cell
rule cfg grid = case (health curCell) of
  Healthy 0  -> maybeInfectCell cfg grid
  Ill 0      -> curCell { health = Healthy $ immunityDuration cfg }
  Infected 0 -> curCell { health = Ill $ illnessDuration cfg }
  _          -> curCell { health = oneDayTick $ health curCell}
  where
    curCell = extract grid

evolve :: Cfg -> Grid Cell -> Grid Cell
evolve cfg = extend (rule cfg)

comonad19 :: Cfg -> Int -> IO ()
comonad19 cfg steps = do
  let grids = take steps $ iterate (evolve cfg) (initialGrid cfg)
  forM_ grids print

sampleCfg :: Cfg
sampleCfg = Cfg 0.4 1 2 3