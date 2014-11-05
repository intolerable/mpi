module Main where

import Matrix (Matrix, (!), (<->))

import Control.Monad
import Data.Foldable (foldlM)
import Data.Vector.Unboxed (Unbox)
import System.Exit
import qualified Control.Parallel.MPI.Simple as MPI
import qualified Matrix

globalL :: Int
globalL = 4

iterations :: Int
iterations = 100

main :: IO ()
main = MPI.mpiWorld $ \size rank -> do
  unless ((globalL * globalL) `mod` size == 0) $ do
    putStrLn "Incorrect number of processes for L (L must be cleanly divisible by N)"
    exitFailure
  let a = matrixA globalL :: Matrix Double
  let b = initialB globalL :: Matrix Double
  case fromIntegral rank of
    0 -> do
      let
        go m _ = do
          MPI.bcastSend MPI.commWorld 0 m
          xs <- MPI.gatherRecv MPI.commWorld 0 $
            jacobi size 0 a b m
          return $ foldl1 (<->) xs
      res <- foldlM go b [1..iterations]
      putStrLn $ Matrix.prettyMatrix res
    r -> do
      let
        go () _ = do
          x <- MPI.bcastRecv MPI.commWorld 0
          MPI.gatherSend MPI.commWorld 0 $
            jacobi size r a b x
      foldlM go () [1..iterations]

splitMatrix :: Unbox a => Int -> Matrix a -> [Matrix a]
splitMatrix size m = map (split m) [0..size-1]
  where rowsPerBlock = Matrix.rows m `div` size
        nCols = Matrix.columns m
        startRow b = (b * rowsPerBlock) + 1
        endRow b = ((b + 1) * rowsPerBlock)
        split matrix b = Matrix.submatrix (startRow b) (endRow b) 1 nCols matrix

splitMatrixVertically :: Unbox a => Int -> Matrix a -> [Matrix a]
splitMatrixVertically size m = map (split m) [0..size-1]
  where rowsPerBlock = Matrix.rows m `div` size
        nRows = Matrix.columns m
        startRow b = (b * rowsPerBlock) + 1
        endRow b = ((b + 1) * rowsPerBlock)
        split matrix b = Matrix.submatrix 1 nRows (startRow b) (endRow b) matrix

matrixA :: (Unbox a, Num a) => Int -> Matrix a
matrixA l = Matrix.matrix n n (generator l)
  where n = l * l

diagonal :: (Unbox a, Num a) => Matrix a -> Matrix a
diagonal m = Matrix.matrix (Matrix.columns m) (Matrix.rows m) gen
  where
    gen i j | i == j = m ! (i, j)
    gen _ _ = 0

inverse :: (Unbox a, Fractional a) => Matrix a -> Matrix a
inverse m = Matrix.matrix (Matrix.columns m) (Matrix.rows m) gen
  where
    gen i j | i == j = 1 / (m ! (i, j))
    gen _ _ = 0

inverseDiag :: (Unbox a, Fractional a) => Matrix a -> Matrix a
inverseDiag = inverse . diagonal

generator :: Num b => Int -> Int -> Int -> b
generator _ i j | i == j = -4
generator _ i j | i == j + 1 = 1
generator _ i j | i == j - 1 = 1
generator l i j | i == j + l = 1
generator l i j | i == j - l = 1
generator _ _ _ = 0

initialB :: (Unbox a, Num a) => Int -> Matrix a
initialB l = Matrix.matrix n 1 gen
  where gen _ _ = 1
        n = l * l

jacobi :: (Show a, Unbox a, Fractional a) => Int -> Int -> Matrix a -> Matrix a -> Matrix a -> Matrix a
jacobi size rank a b x =
  Matrix.matrix (Matrix.rows x `div` 2) 1 $ \i _ ->
    let i' = ((Matrix.rows x `div` size) * rank) + i in
    let s = sum [ a ! (i', j) * x ! (j, 1) | j <- [1 .. Matrix.rows x], i' /= j] in
    (1 / (a ! (i', i'))) * ((b ! (i', 1)) - s)
