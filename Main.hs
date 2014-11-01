module Main where

import Matrix (Matrix, (!))
import Data.Vector.Unboxed (Unbox)
import qualified Control.Parallel.MPI.Simple as MPI
import qualified Matrix

main :: IO ()
main = MPI.mpiWorld $ \_size _rank -> do
  let a = (matrixA 4 :: Matrix Double)
  let b = initialB 4
  let d' = inverseDiag a
  let final = iterate (jacobi a b d') b
  putStrLn $ Matrix.prettyMatrix $ final !! 50000

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

generator :: (Num a, Eq a, Num b) => a -> a -> a -> b
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

jacobi :: (Unbox a, Fractional a) => Matrix a -> Matrix a -> Matrix a -> Matrix a -> Matrix a
jacobi a b d' x = x + (d' * (b - (a * x)))
