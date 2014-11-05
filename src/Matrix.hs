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

map :: (Unbox a, Unbox b) => (a -> b) -> Matrix a -> Matrix b
map f m = Matrix $ M.map f $ unMatrix m

toList :: Unbox a => Matrix a -> [a]
toList = M.toList . unMatrix

(!) :: Unbox a => Matrix a -> (Int, Int) -> a
(!) = (M.!) . unMatrix

submatrix :: Unbox a => Int -> Int -> Int -> Int -> Matrix a -> Matrix a
submatrix r1 r2 c1 c2 = Matrix . M.submatrix r1 r2 c1 c2 . unMatrix

(<|>) :: Unbox a => Matrix a -> Matrix a -> Matrix a
m1 <|> m2 = Matrix $ unMatrix m1 M.<|> unMatrix m2

(<->) :: Unbox a => Matrix a -> Matrix a -> Matrix a
m1 <-> m2 = Matrix $ unMatrix m1 M.<-> unMatrix m2
