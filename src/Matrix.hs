module Matrix where

import Control.Applicative
import Data.Serialize (Serialize)
import Data.Foldable (Foldable)
import qualified Data.Vector as V
import qualified Data.Matrix as M
import qualified Data.Serialize as Serialize

newtype Matrix a = Matrix { unMatrix :: M.Matrix a }
  deriving (Show, Eq, Num, Functor, Foldable)

instance Serialize a => Serialize (Matrix a) where
  put = Serialize.put . M.toLists . unMatrix
  get = Matrix . M.fromLists <$> Serialize.get

onMatrix :: (M.Matrix a -> M.Matrix a) -> Matrix a -> Matrix a
onMatrix f = Matrix . f . unMatrix

identity :: Num a => Int -> Matrix a
identity = Matrix . M.identity

prettyMatrix :: Show a => Matrix a -> String
prettyMatrix = M.prettyMatrix . unMatrix

transpose :: Matrix a -> Matrix a
transpose = onMatrix M.transpose

scaleMatrix :: Num a => a -> Matrix a -> Matrix a
scaleMatrix s = onMatrix (M.scaleMatrix s)

get :: Int -> Int -> Matrix a -> Maybe a
get i j m = M.safeGet i j $ unMatrix m

matrix :: Int -> Int -> (Int -> Int -> a) -> Matrix a
matrix i j f = Matrix $ M.matrix i j (uncurry f)

diagonal :: Matrix a -> V.Vector a
diagonal = M.getDiag . unMatrix

columns :: Matrix a -> Int
columns = M.ncols . unMatrix

rows :: Matrix a -> Int
rows = M.nrows . unMatrix

(!) :: Matrix a -> (Int, Int) -> a
(!) = (M.!) . unMatrix

multStd :: Num a => Matrix a -> Matrix a -> Matrix a
multStd m n = Matrix $ unMatrix m `M.multStd` unMatrix n

multStd2 :: Num a => Matrix a -> Matrix a -> Matrix a
multStd2 m n = Matrix $ unMatrix m `M.multStd2` unMatrix n

multStrassen :: Num a => Matrix a -> Matrix a -> Matrix a
multStrassen m n = Matrix $ unMatrix m `M.multStrassen` unMatrix n

multStrassenMixed :: Num a => Matrix a -> Matrix a -> Matrix a
multStrassenMixed m n = Matrix $ unMatrix m `M.multStrassenMixed` unMatrix n