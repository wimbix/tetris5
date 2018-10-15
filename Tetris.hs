-- | The Tetris game (main module)
module Main where
import ConsoleGUI       -- cabal install ansi-terminal
--import CodeWorldGUI     -- cabal install codeworld-api
import Shapes
import Test.QuickCheck
import Data.List

--------------------------------------------------------------------------------
-- * The code that puts all the piece together

main = runGame tetrisGame

tetrisGame = Game { startGame     = startTetris,
                    stepGame      = stepTetris,
                    drawGame      = drawTetris,
                    gameInfo      = defaultGameInfo prop_Tetris,
                    tickDelay     = defaultDelay,
                    gameInvariant = prop_Tetris }

--------------------------------------------------------------------------------
-- * The various parts of the Tetris game implementation


-- | The state of the game
data Tetris = Tetris (Vector,Shape) Shape [Shape]
-- The state consists of three parts:
--   * The position and shape of the falling piece
--   * The well (the playing field), where the falling pieces pile up
--   * An infinite supply of random shapes

-- ** Positions and sizes

type Vector = (Int,Int)

-- | The size of the well
wellSize :: (Int,Int)
wellSize = (wellWidth,wellHeight)
wellWidth = 10
wellHeight = 20

-- | Starting position for falling pieces
startPosition :: Vector
startPosition = (wellWidth `div` 2 - 1, 0)

-- | Vector addition
vAdd :: Vector -> Vector -> Vector
(x1,y1) `vAdd` (x2,y2) = (x1+x2,y1+y2)

-- | Move the falling piece into position
place :: (Vector,Shape) -> Shape
place (v,s) = shiftShape v s

-- ** B4
-- | An invariant that startTetris and stepTetris should uphold
prop_Tetris :: Tetris -> Bool
prop_Tetris (Tetris (_,s) w _) = shapeSize w == (10,20) && prop_Shape s

-- ** B5
-- | Add black walls around a shape
addWalls :: Shape -> Shape
addWalls s = S ((map (Just Black:) (shape))
 ++ ([replicate (n+2) (Just Black)]))
  where n = length $ head $ rows s
        hor = replicate n (Just Black):(rows s)
        shape = map (++[Just Black]) (hor)

-- ** B6
-- | Visualize the current game state. This is what the user will see
-- when playing the game.
drawTetris :: Tetris -> Shape
drawTetris (Tetris (v,p) w supply) = addWalls(comb)
   where comb = w `combine` (shiftShape v p)

-- | The initial game state
startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition,shape1) (emptyShape wellSize) supply
  where
    shape1:supply = map (allShapes!!) (rs' rs)
      where rs'(x:xs) = floor (x*6) : rs' xs

-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
stepTetris :: Action -> Tetris -> Maybe (Int,Tetris)
stepTetris MoveRight t = Just (0,(movePiece 1 t))
stepTetris MoveLeft  t = Just (0,(movePiece (-1) t))
stepTetris MoveDown  t = tick t
stepTetris Rotate    t = Just (0,(rotatePiece t))
stepTetris Tick      t = tick t

move :: Vector -> Tetris -> Tetris
move v (Tetris (v1,p) w supply) = (Tetris (v1 `vAdd` v, p) w supply)

tick :: Tetris -> Maybe (Int,Tetris)
tick t | collision newState == False = Just (0, move (0,1) t)
       | collision t1       == False = dropNewPiece t
       | otherwise          = Nothing
   where newState    = move (0,1) t
         Just (n,t1) = dropNewPiece t

collision :: Tetris -> Bool
collision (Tetris (v,p) w supply) = y>(20-t) || x>(10-r) || 0>x || place (v,p) `overlaps` w
 where (x,y) = v
       (r,t) = shapeSize p

movePiece :: Int -> Tetris -> Tetris
movePiece n t | collision newState == False = newState
              | otherwise                   = t
 where newState = move (n,0) t

rotate :: Tetris -> Tetris
rotate (Tetris (v,p) w supply) = (Tetris (v,rotateShape p) w supply)

rotatePiece :: Tetris -> Tetris
rotatePiece t | collision newState == False = newState
              | otherwise                   = t
  where newState = rotate t

dropNewPiece :: Tetris -> Maybe (Int,Tetris)
dropNewPiece t = Just (n,(Tetris (startPosition,(head supply)) w1 (tail(supply))))
  where newWell = (w `combine` (place (v,p)))
        (n,w1) = clearLines (newWell)
        Tetris (v,p) w supply = t

clearLines :: Shape -> (Int,Shape)
clearLines w = (n,(S (replicate n (replicate 10 Nothing)++newWell)))
   where n = 20 - (length (newWell))
         newWell = filter isComplete(rows w)

isComplete :: Row -> Bool
isComplete r = length (filter (/=Nothing) r) /= 10
