module Matrix where

import Control.Applicative
import Data.Serialize (Serialize)
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as V
import qualified Data.Matrix.Unboxed as M
import qualified Data.Serialize as Serialize

newtype Matrix a = Matrix { unMatrix :: M.Matrix a }
  deriving (Show, Eq, Num)

instance (Unbox a, Serialize a) => Serialize (Matrix a) where
  put = Serialize.put . M.toLists . unMatrix
  get = Matrix . M.fromLists <$> Serialize.get

onMatrix :: Unbox a => (M.Matrix a -> M.Matrix a) -> Matrix a -> Matrix a
onMatrix f = Matrix . f . unMatrix

identity :: (Unbox a, Num a) => Int -> Matrix a
identity = Matrix . M.identity

prettyMatrix :: (Unbox a, Show a) => Matrix a -> String
prettyMatrix = M.prettyMatrix . unMatrix

transpose :: Unbox a => Matrix a -> Matrix a
transpose = onMatrix M.transpose

scaleMatrix :: (Unbox a, Num a) => a -> Matrix a -> Matrix a
scaleMatrix s = onMatrix (M.scaleMatrix s)

get :: Unbox a => Int -> Int -> Matrix a -> Maybe a
get i j m = M.safeGet i j $ unMatrix m

matrix :: Unbox a => Int -> Int -> (Int -> Int -> a) -> Matrix a
matrix i j f = Matrix $ M.matrix i j (uncurry f)

diagonal :: Unbox a => Matrix a -> V.Vector a
diagonal = M.getDiag . unMatrix

columns :: Unbox a => Matrix a -> Int
columns = M.ncols . unMatrix

rows :: Unbox a => Matrix a -> Int
rows = M.nrows . unMatrix

(!) :: Unbox a => Matrix a -> (Int, Int) -> a
(!) = (M.!) . unMatrix

multStd :: (Unbox a, Num a) => Matrix a -> Matrix a -> Matrix a
multStd m n = Matrix $ unMatrix m `M.multStd` unMatrix n

multStd2 :: (Unbox a, Num a) => Matrix a -> Matrix a -> Matrix a
multStd2 m n = Matrix $ unMatrix m `M.multStd2` unMatrix n

multStrassen :: (Unbox a, Num a) => Matrix a -> Matrix a -> Matrix a
multStrassen m n = Matrix $ unMatrix m `M.multStrassen` unMatrix n

multStrassenMixed :: (Unbox a, Num a) => Matrix a -> Matrix a -> Matrix a
multStrassenMixed m n = Matrix $ unMatrix m `M.multStrassenMixed` unMatrix n
