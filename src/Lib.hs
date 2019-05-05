{-# LANGUAGE MultiWayIf #-}
module Lib
    ( someFunc
    ) where

import qualified Data.Vector.Unboxed as VU
import Linear
import Control.Monad.Random
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

someFunc :: IO ()
someFunc = chartSteps 7

prepost :: V2 Double -> V2 Double
prepost a = (1/3) *^ a

insertRand :: MonadRandom m => V2 Double -> m (V2 Double,V2 Double)
insertRand a = do
  invert <- getRandom
  if invert
    then return $ (rotateV2 (pi/3) (prepost a), rotateV2 (5*(pi/3)) (prepost a))
    else return $ (rotateV2 (5*(pi/3)) (prepost a), rotateV2 (pi/3) (prepost a))

rotateV2 :: Double -> V2 Double -> V2 Double
rotateV2 angle v = mat !* v
  where
    mat = V2 (V2 (cos angle) ((-1) * sin angle)) (V2 (sin angle) (cos angle))

step :: MonadRandom m => VU.Vector (V2 Double) -> m (VU.Vector (V2 Double))
step v = VU.fromList <$> build 0
  where
    n = VU.length v
    n' = 4 * n
    build i =
      if | i == n' -> return []
         | i `mod` 4 == 0 || i `mod` 4 == 3 -> do
             rest <- build $ i + 1
             return $ (prepost $ (VU.!) v (i `div` 4)):rest
         | i `mod` 4 == 1 -> do
             rest <- build $ i + 2
             (a,b) <- insertRand $ (VU.!) v (i `div` 4)
             return $ a:b:rest
         | otherwise -> error "Impossible case"

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

chartSteps :: Int -> IO ()
chartSteps n = do
  sts <- evalRandIO $ steps n

  let fileOpts = FileOptions (1024,768) PDF

  toFile fileOpts "fractal.pdf" $ do
    layout_title .= "Fractal (" ++ show n ++ " levels)"
    plot (line "Points" [(0,0):(toDisplay $ head sts)])
  return ()

toDisplay :: VU.Vector (V2 Double) -> [(Double, Double)]
toDisplay v = fmap (\(V2 a b) -> (a,b)) $ VU.toList $ VU.scanl1 (+) v 