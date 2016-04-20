module Math
  ( Ray(..)
  , RaySegment (..)
  , V3d, P3d, p3d, vp3d
  , UnitV3d, unitV3d, vunitV3d, unitV3map, liftUnitV3
  , normalized, mkTangent
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


farthestDistance :: Double
farthestDistance = 1e+10

type V3d = V3 Double
type P3d = Point V3 Double
-- ^ World coordinate system

p3d :: Double -> Double -> Double -> P3d
p3d x y z = P $ V3 x y z

vp3d :: V3d -> P3d
vp3d = P

newtype Ray = Ray (P3d, UnitV3d)
  deriving Show
-- ^ position & direction

newtype RaySegment = RaySeg (Ray, Double)
  deriving Show
-- ^ ray segment over Ray and between [0..Double]

---------------------------------------------------------------------
-- | Track normalized type-safe vectors in world-coordinate system
newtype UnitV3d = UnitV3 V3d deriving (Eq, Show)

unitV3map :: (V3d -> V3d) -> UnitV3d -> UnitV3d
f `unitV3map` UnitV3 x = UnitV3 . normalize $ f x

liftUnitV3 :: (V3d -> V3d -> V3d) -> UnitV3d -> UnitV3d -> UnitV3d
liftUnitV3 f (UnitV3 u) (UnitV3 v) = UnitV3 . normalize $ f u v

normalized :: UnitV3d -> V3d
normalized (UnitV3 v) = v

unitV3d :: Double -> Double -> Double -> UnitV3d
unitV3d x y z = UnitV3 . normalize $ V3 x y z

vunitV3d :: V3d -> UnitV3d
vunitV3d = UnitV3 . normalize

---------------------------------------------------------------------
clamp :: forall a. Ord a => a -> a -> a -> a
clamp min' max' = min max' . max min'

-- | Convert random Word32 numbers to a Double in the range [0,1]
inRange :: RandomGen g => g -> Int -> Double
inRange gen i = on (P./) fromIntegral (i - min') (max' - min')
  where (min', max') = genRange gen

mkTangent :: UnitV3d -> UnitV3d
mkTangent (UnitV3 (V3 x y z)) = vunitV3d result
  where result | x' <= y' && x' <= z' = V3 0 z (-y)
               | y' <= z'             = V3 z 0 (-x)
               | otherwise            = V3 y (-x) 0
        (x', y', z') = (abs x, abs y, abs z)

---------------------------------------------------------------------
-- | length, theta (the elevation angle in the range [-pi/2, pi/2])
--  and phi (the azimuth angle in the range [0, 2*pi])
data SphericalVec = SphereV !Double !(PlaneAngle Double) !(PlaneAngle Double)

toSpherical :: V3d -> SphericalVec
toSpherical v@(V3 x y z) = SphereV r theta phi
  where r     = norm v
        theta = asin (z P./ r *~ one)
        phi   = if phi' < _0 then phi' + (_2*pi) else phi'
        phi'  = atan2 (y *~ one) (x *~ one)

-- | theta The elevation angle in the range [-pi/2, pi/2].
--  phi The azimuth angle in the range [0, 2*pi].
fromSpherical :: SphericalVec -> V3d
fromSpherical (SphereV r theta phi) = r *^ V3 x y z
  where x = cos phi * thetaCos /~ one
        y = sin phi * thetaCos /~ one
        z = sin theta /~ one
        thetaCos = cos theta
