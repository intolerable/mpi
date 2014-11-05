module Main (main) where

import Matrix (Matrix, (!), (<->))

import Control.Monad
import Data.Vector.Unboxed (Unbox)
import System.Exit
import qualified Control.Parallel.MPI.Simple as MPI
import qualified Matrix

globalL :: Int
globalL = 4

tolerance :: Double
tolerance = 0.01

main :: IO ()
main = MPI.mpiWorld $ \size rank -> do
  unless ((globalL * globalL) `mod` size == 0) $ do
    putStrLn "Incorrect number of processes for L (L must be cleanly divisible by N)"
    exitFailure
  let a = matrixA globalL :: Matrix Double
  let b = initialB globalL :: Matrix Double
  t <- MPI.wtime
  case size of
    1 -> do
      let solutions = iterate (jacobiSerial a b (inverseDiag a)) b
      putStrLn $ Matrix.prettyMatrix $ snd $ head $
        dropWhile (\(x,y) -> errorFunction x y > (tolerance ** 2)) (solutions `zip` tail solutions)
      finishT <- MPI.wtime
      print $ finishT - t
    _ ->
      do
        let
          go x = do
            xs <- MPI.allgather MPI.commWorld $
              jacobi size (fromIntegral rank) a b x
            let newX = foldl1 (<->) xs
            if errorFunction x newX < (tolerance ** 2)
              then return newX
              else go newX
        res <- go b
        when (rank == 0) $ do
          putStrLn $ Matrix.prettyMatrix res
          finishT <- MPI.wtime
          print $ finishT - t

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
  Matrix.matrix (Matrix.rows x `div` size) 1 $ \i _ ->
    let i' = ((Matrix.rows x `div` size) * rank) + i in
    let s = sum [ a ! (i', j) * x ! (j, 1) | j <- [1 .. Matrix.rows x], i' /= j] in
    (1 / (a ! (i', i'))) * ((b ! (i', 1)) - s)

jacobiSerial :: (Unbox a, Fractional a) => Matrix a -> Matrix a -> Matrix a -> Matrix a -> Matrix a
jacobiSerial a b d' x = x + (d' * (b - (a * x)))

errorFunction :: (Unbox a, Fractional a, Floating a) => Matrix a -> Matrix a -> a
errorFunction m1 m2 = sum $ Matrix.toList $ Matrix.map (**2) $ m1 - m2
