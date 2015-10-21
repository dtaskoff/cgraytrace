{-# LANGUAGE FlexibleInstances #-}
module BRDF where

import qualified Prelude as P
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional
import Geometry
import Light
import Math
import Linear
import System.Random (RandomGen(..))
import Control.Applicative

-- theta The elevation angle in the range [-pi/2, pi/2]
-- phi The azimuth angle in the range [0, 2*pi]
data SphericalVec = SphereV !Float !(PlaneAngle Float) !(PlaneAngle Float) -- length, theta, phi

toSpherical :: V3 Float -> SphericalVec
toSpherical v@(V3 x y z) = SphereV r theta phi where
        r     = norm v
        theta = asin (Dimensional$ z P./ r)
        phi'  = atan2 (Dimensional y) (Dimensional x)
        phi   = if phi' < _0 then phi' + (_2*pi) else phi'

-- theta The elevation angle in the range [-pi/2, pi/2].
-- phi The azimuth angle in the range [0, 2*pi].
fromSpherical :: SphericalVec -> V3 Float
fromSpherical (SphereV r theta phi) = r *^ V3 (cos phi * thetaCos /~ one) (sin phi * thetaCos /~ one) (sin theta /~ one) where
        thetaCos = cos theta

class BRDF brdf geom where
    evalBRDF     :: brdf -> Intersection geom -> Normal -> Normal -> Albedo -- intersection info, dir to viewer, dir to light
    generateRay  :: RandomGen gen => gen -> brdf -> Intersection geom -> (Ray, gen) -- generate new ray reflected/refracted from the surface

data BRDFs = Diffuse Albedo
                deriving (Eq, Show)

transfer :: Float -> Float -> Float -> Albedo
transfer r g b = V3 (r *~one) (g *~one) (b *~one)

instance BRDF BRDFs a where
    evalBRDF (Diffuse reflectivity) (Hit _ _ inormal _) _ dir2light =
        (cs' *) <$> reflectivity where
            csL = dot (normalized inormal) (normalized dir2light) *~ one
            cs' = _2 * clamp _0 _1 csL

    generateRay gen (Diffuse _) (Hit _ ipoint inormal _) = (Ray (ipoint, normalize3 dir), gen'') where
        (ran_theta, gen') = next gen
        (ran_phi, gen'')  = next gen'
        inRange' g x = Dimensional $ inRange g x                        :: PlaneAngle Float

        SphereV _ theta phi = toSpherical . normalized $ inormal
        theta' = inRange' gen ran_theta * (pi / _2) - (pi / _4) + theta :: PlaneAngle Float
        phi'   = inRange' gen' ran_phi * pi - (pi / _2) + phi           :: PlaneAngle Float
        dir    = fromSpherical( SphereV 1 theta' phi' )