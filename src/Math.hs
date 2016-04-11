module Math
  ( Ray(..)
  , RaySegment (..)
  , Vec3, Coord3, coord
  , UnitVec3, unitvec, normalize3, normalized, mkTangent
  , SphericalVec(..), toSpherical, fromSpherical
  , clamp, inRange
  , farthestDistance
  ) where

import Prelude (abs, (-))
import qualified Prelude as P
import Data.Function (on)
import Numeric.Units.Dimensional.Prelude hiding (abs, (-))
import Linear
import Linear.Affine
import System.Random (RandomGen(..))


farthestDistance :: Float
farthestDistance = 1e+10

type Vec3   = V3 Float
type Coord3 = Point V3 Float
-- ^World coordinate system

coord :: a -> a -> a -> Point V3 a
coord x y z = P $ V3 x y z

newtype Ray = Ray (Coord3, UnitVec3)
  deriving Show
-- ^position & direction

newtype RaySegment = RaySeg (Ray, Float)
  deriving Show
-- ^ray segment over Ray and between [0..Float]

---------------------------------------------------------------------
-- |Track normalized type-safe vectors in world-coordinate system
newtype UnitV3 a = UnitV3 a
  deriving (Eq, Show)
type UnitVec3 = UnitV3 Vec3

instance Functor UnitV3 where
  f `fmap` UnitV3 x = UnitV3 $ f x

instance Applicative UnitV3 where
  pure = UnitV3
  UnitV3 f <*> x = f <$> x

normalize3 :: Vec3 -> UnitVec3
normalize3 = pure . normalize

normalized :: UnitVec3 -> Vec3
normalized (UnitV3 v) = v

unitvec :: Float -> Float -> Float -> UnitVec3
unitvec x y z = normalize3 $ V3 x y z

---------------------------------------------------------------------
clamp :: forall a. Ord a => a -> a -> a -> a
clamp min' max' = min max' . max min'

-- |Convert random Word32 numbers to Float in the range [0,1]
inRange :: RandomGen g => g -> Int -> Float
inRange gen i = on (P./) fromIntegral (i - min') (max' - min')
  where (min', max') = genRange gen

mkTangent :: UnitVec3 -> UnitVec3
mkTangent (UnitV3 (V3 x y z)) = normalize3 result
  where result | x' <= y' && x' <= z' = V3 0 z (-y)
               | y' <= z'             = V3 z 0 (-x)
               | otherwise            = V3 y (-x) 0
        (x', y', z') = (abs x, abs y, abs z)

---------------------------------------------------------------------
-- |length, theta (the elevation angle in the range [-pi/2, pi/2])
--  and phi (the azimuth angle in the range [0, 2*pi])
data SphericalVec = SphereV !Float !(PlaneAngle Float) !(PlaneAngle Float)

toSpherical :: Vec3 -> SphericalVec
toSpherical v@(V3 x y z) = SphereV r theta phi
  where r     = norm v
        theta = asin (z P./ r *~ one)
        phi   = if phi' < _0 then phi' + (_2*pi) else phi'
        phi'  = atan2 (y *~ one) (x *~ one)

-- |theta The elevation angle in the range [-pi/2, pi/2].
--  phi The azimuth angle in the range [0, 2*pi].
fromSpherical :: SphericalVec -> Vec3
fromSpherical (SphereV r theta phi) = r *^ V3 x y z
  where x = cos phi * thetaCos /~ one
        y = sin phi * thetaCos /~ one
        z = sin theta /~ one
        thetaCos = cos theta
