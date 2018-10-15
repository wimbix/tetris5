-- | Types and functions for shapes. The list of all tetris pieces.
module Shapes where
import Test.QuickCheck
import Data.List(transpose)
import Data.Maybe(isNothing)

-- * Shapes

type Square = Maybe Colour

data Colour = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey
              deriving (Eq,Bounded,Enum,Show)

-- | A geometric shape is represented as a list of lists of squares. Each square
-- can be empty or filled with a block of a specific colour.

data Shape = S [Row] deriving (Eq)
type Row = [Square]

rows :: Shape -> [Row]
rows (S rs) = rs

-- * Showing shapes

showShape :: Shape -> String
showShape s = unlines [showRow r | r <- rows s]
  where
    showRow :: Row -> String
    showRow r = [showSquare s | s <- r]

    showSquare Nothing = '.'
    showSquare (Just Black) = '#' -- can change to '█' on linux/mac
    showSquare (Just Grey)  = 'g' -- can change to '▓'
    showSquare (Just c)     = head (show c)

instance Show Shape where
  show = showShape
  showList ss r = unlines (map show ss)++r


-- * The shapes used in the Tetris game

-- | All 7 tetrominoes (all combinations of connected 4 blocks),
-- see <https://en.wikipedia.org/wiki/Tetromino>
allShapes :: [Shape]
allShapes = [S (makeSquares s) | s <- shapes]
   where
      makeSquares = map (map colour)
      colour c    = lookup c [('I',Red),('J',Grey),('T',Blue),('O',Yellow),
                              ('Z',Cyan),('L',Green),('S',Purple)]
      shapes =
              [["I",
               "I",
               "I",
               "I"],
              [" J",
               " J",
               "JJ"],
              [" T",
               "TT",
               " T"],
              ["OO",
               "OO"],
              [" Z",
               "ZZ",
               "Z "],
              ["LL",
               " L",
               " L"],
              ["S ",
               "SS",
               " S"]]

-- * Some simple functions

-- ** A01
emptyShape :: (Int,Int) -> Shape
emptyShape (y,x) = S (replicate x (emptyShape' y))
emptyShape' :: Int -> Row
emptyShape' n = replicate n Nothing

-- ** A02

-- | The size (width and height) of a shape
shapeSize :: Shape -> (Int,Int)
shapeSize (S []) = (0,0)
shapeSize s = (x, y)
   where y = length (rows s)
         x = length (head (rows s))

-- ** A03
-- | Count how many non-empty squares a shape contains
blockCount :: Shape -> Int
blockCount s = length (filter (notEmpty) (concat (rows s)))

notEmpty s | s == Nothing = False
           | otherwise = True


-- * The Shape invariant

-- ** A04
-- | Shape invariant (shapes have at least one row, at least one column,
-- and are rectangular)
prop_Shape :: Shape -> Bool
prop_Shape s = x >= 1 && y >= 1 && rect s
    where (x,y) = shapeSize s

rect :: Shape -> Bool
rect s = all (==length (head (rows s))) (map length (rows s))

-- * Test data generators

-- ** A05
-- | A random generator for colours
rColour :: Gen Colour
rColour = elements [Black, Red, Green, Yellow, Blue, Purple, Cyan, Grey]

instance Arbitrary Colour where
   arbitrary = rColour

-- ** A06
-- | A random generator for shapes
rShape :: Gen Shape
rShape = elements allShapes

instance Arbitrary Shape where
    arbitrary = rShape

-- * Transforming shapes

-- ** A07
-- | Rotate a shape 90 degrees
rotateShape :: Shape -> Shape
rotateShape s = (S (reverse ((transpose (rows s)))))

-- ** A08
-- | shiftShape adds empty squares above and to the left of the shape
shiftShape :: (Int,Int) -> Shape -> Shape
shiftShape (y,x) s = S (hor++ver)
     where hor = replicate x (replicate n Nothing)
           ver = add (replicate y Nothing) (rows s)
           n = length (head (ver))
           add z = map (z++)

-- ** A09
-- | padShape adds empty sqaure below and to the right of the shape
padShape :: (Int,Int) -> Shape -> Shape
padShape (y,x) s = S (ver++hor)
     where hor = replicate x (replicate n Nothing)
           ver = add (replicate y Nothing) (rows s)
           n = length (head (ver))
           add z = map (++z)

-- ** A10
-- | pad a shape to a given size
padShapeTo :: (Int,Int) -> Shape -> Shape
padShapeTo (x,y) s = padShape ((x-r),(y-t)) s
   where (r,t) = shapeSize s

-- * Comparing and combining shapes

-- ** B1

-- | Test if two shapes overlap
overlaps :: Shape -> Shape -> Bool
s1 `overlaps` s2 = or [rowsOverlap ((rows s1)!!n) ((rows s2)!!n)
   | n <- [0..(x-1)]]
    where x = min (length (rows s1)) (length (rows s2))

rowsOverlap :: Row -> Row -> Bool
rowsOverlap r1 r2 = or [r1!!n/=Nothing && r2!!n/=Nothing
  | n <- [0..(x-1)]]
    where x = min (length r1) (length r2)

-- ** B2
-- | zipShapeWith, like 'zipWith' for lists
zipShapeWith :: (Square->Square->Square) -> Shape -> Shape -> Shape
zipShapeWith f (S s1) (S s2) = S (zipShapeWith' f s1 s2)
zipShapeWith' f _ []  = []
zipShapeWith' f [] _  = []
zipShapeWith' f (x:xs) (y:ys) = (zipWith f x y) : (zipShapeWith' f xs ys)

-- ** B3
-- | Combine two shapes. The two shapes should not overlap.
-- The resulting shape will be big enough to fit both shapes.
combine :: Shape -> Shape -> Shape
s1 `combine` s2 = combine' shape1 shape2
  where shape1  = padShapeTo (x,y) s1
        shape2  = padShapeTo (x,y) s2
        x       = max (length $ head $ rows $ s1) (length $ head $ rows $ s2)
        y       = max (length $ rows $ s1) (length $ rows $ s2)

combine' :: Shape -> Shape -> Shape
combine' s1 s2 = zipShapeWith add s1 s2
          where add :: Square -> Square -> Square
                add Nothing Nothing = Nothing
                add Nothing s       = s
                add s       Nothing = s
                add (Just c1) (Just c2) = (Just c1)
