{-# LANGUAGE MultiWayIf #-}
module Lib
    ( chartSteps
    ) where

import           Control.Monad.Random
import           Control.Monad.Trans.State
import qualified Data.Vector.Unboxed as VU
import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy
import           Linear

-- | Cut a vector to a third of it's length
thirdlen :: V2 Double -> V2 Double
thirdlen a = (1/3) *^ a

-- | For a given vector segments give two vectors that, when added result in the
-- original vector. These vectors will be rotated by 60 degrees to form an
-- equilateral triangle with the original vector.
insertRand :: MonadRandom m => V2 Double -> m (V2 Double,V2 Double)
insertRand a = do
  invert <- getRandom
  if invert
    then return $ ( rotateV2 (pi/3) (thirdlen a)
                  , rotateV2 (5*(pi/3)) (thirdlen a))
    else return $ ( rotateV2 (5*(pi/3)) (thirdlen a)
                  , rotateV2 (pi/3) (thirdlen a))

-- | Rotate a given two dimensional vector by the given angle (polar
-- coordinates)
rotateV2 :: Double -> V2 Double -> V2 Double
rotateV2 angle v = mat !* v
  where
    mat = V2 (V2 (cos angle) ((-1) * sin angle)) (V2 (sin angle) (cos angle))

-- | Perform one randomized initiator-generator cycle of fractal generation.
step :: MonadRandom m => VU.Vector (V2 Double) -> m (VU.Vector (V2 Double))
step v = evalStateT (VU.generateM n' build) Nothing
  where
    n = VU.length v
    n' = 4 * n
    build i =
      if | i `mod` 4 == 0 || i `mod` 4 == 3 -> do
             return $ (thirdlen $ (VU.!) v (i `div` 4))
         | i `mod` 4 == 1 -> do
             (a,b) <- lift $ insertRand $ (VU.!) v (i `div` 4)
             put $ Just b
             return $ a
         | i `mod` 4 == 2 -> do
             xMaybe <- get
             case xMaybe of
               Nothing -> error "Impossible case (1)"
               Just x -> put Nothing >> return x
         | otherwise -> error "Impossible case (2)"

-- | Perform a number of (randomized) initiator generator cycles.
steps :: MonadRandom m => Int -> m [(VU.Vector (V2 Double))]
steps n
  | otherwise = fmap reverse $ go n $ VU.fromList [V2 1 0]
  where
    go i xs
      | i <= 0 = return []
      | i > 0 = do
        this <- step xs
        rest <- go (i - 1) this
        return $ this : rest

-- | Generate a nice pdf-rendered graphic
chartSteps :: Int -> IO ()
chartSteps n = do
  sts <- evalRandIO $ steps n

  let fileOpts = FileOptions (1024,768) PDF
      points = (0,0):(toDisplay $ head sts)
      plot = toPlot
             $ plot_lines_values .~ [points]
             $ def

  renderableToFile fileOpts "fractal.pdf"
    $ toRenderable
    $ layout_title .~ ("Fractal (" ++ show n ++ " levels)")
    $ layout_plots .~ [plot]
    $ layout_y_axis . laxis_generate .~ scaledAxis def ((-0.5),0.5)
    $ layout_x_axis . laxis_generate .~ scaledAxis def (0,1)
    $ def
  return ()

-- | Affine vectors for a regular polygon with n sides. 
regularN :: Int -> [V2 Double]
regularN n
  | n <= 2 = [V2 1 0]
  | otherwise = do
    k <- fromIntegral <$> [0 .. n]
    let rotationAngle = 2*pi*(k/fromIntegral n)
    return $ rotateV2 rotationAngle (V2 1 0)

-- | Turn a set of affine vectors into a path (list of points) by cumulatively
-- adding them up.
toDisplay :: VU.Vector (V2 Double) -> [(Double, Double)]
toDisplay v = fmap (\(V2 a b) -> (a,b)) $ VU.toList $ VU.scanl1 (+) v 
