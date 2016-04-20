module Light where

import qualified Prelude as P
import Numeric.Units.Dimensional.Prelude hiding ((-))
import Math
import Linear
import Linear.Affine
import System.Random (RandomGen(..))
import Control.Applicative (liftA2)


-- | Light description
type Color a        = V3 a
type LightIntensity = Color (LuminousIntensity Double)
-- ^ R, G, B components of directional luminous intensity [candela]
type LightTrans     = Color (Dimensionless Double)
-- ^ R, G, B coefficients of light transmitance [1/steradian]

-- | Light & shadow interface
class Shadow gen light where
  shadowRay :: RandomGen gen => gen -> light -> P3d -> (RaySegment, gen)
  eval      :: light -> UnitV3d -> LightIntensity -- dir2light

-- | Supported light types
data Light = OmniLight (P3d,           LuminousFlux Double)
            -- ^ center, luminous flux [lumens]
           | RectLight (P3d, V3d, V3d, LuminousFlux Double)
            -- ^ center, side0, side1, luminous flux [lumens]
  deriving Show

-- | Implementation of lights
instance Shadow gen Light where
  shadowRay gen (OmniLight (pos, _)) point' =
      (RaySeg (Ray (point', dir), dist), gen)
    where dir       = vunitV3d vec2light
          dist      = norm vec2light
          vec2light = pos .-. point'

  shadowRay gen (RectLight (ptC, side0, side1, _)) point' =
      (RaySeg (Ray (point', dir), dist), gen'')
    where dir          = vunitV3d vec2light
          dist         = norm vec2light
          vec2light    = pt .-. point'
          pt           = ptC .+^ (vpt0 P.+ vpt1)
          (vpt0, vpt1) = (side0 ^* sampleX, side1 ^* sampleY)
          (sampleX, sampleY) = (inRange gen ran_x P.- 0.5, inRange gen' ran_y P.- 0.5)
          (ran_x, gen')  = next gen
          (ran_y, gen'') = next gen'

  eval (OmniLight (_, e)) _ = V3 x x x
    where x   = e * pi4
          pi4 = _1 / (_4*pi)

  eval (RectLight (_, side0, side1, e)) _ = V3 x x x
    where x       = e * k
          k       = _1 / (_4*pi*surface)               -- double sided
          surface = (norm side0 P.* norm side1) *~ one -- TODO [m^2]


---------------------------------------------------------------------
-- Utilities

zeroLightIntensity :: LightIntensity
zeroLightIntensity = V3 _0 _0 _0

attenuateWith :: LightIntensity -> LightTrans -> LightIntensity
attenuateWith i a = liftA2 (*) i a

averageIntensity :: [LightIntensity] -> LightIntensity
averageIntensity xs = (*invLength) <$> foldl (liftA2 (+)) zeroLightIntensity xs
  where invLength = _1 / (fromIntegral (length xs) *~ one)

transfer :: Double -> Double -> Double -> LightTrans
transfer r g b = V3 (r *~ one) (g *~ one) (b *~ one)
