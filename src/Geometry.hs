module Geometry where

import Math
import Linear
import Linear.Affine
import Control.Applicative (liftA2)


-- |Intersection information returned from each successful intersection
data Intersection g = Hit
  { isectDepth     :: Float    -- depth from ray origin to intersection
  , isectPoint     :: Coord3   -- point of intersection
  , isectNormal    :: UnitVec3 -- normal at the point of intersection
  , isectTangent   :: UnitVec3 -- tangent over the surface at the point of intersection
  , isectBiTangent :: UnitVec3 -- bi-tangent over the surface at the point of intersection
  , isectEntity    :: g        -- intersected geometry
  } deriving Show

class Intersectable geom where
  intersect :: RaySegment -> geom -> Maybe (Intersection geom)

-- |All geometric types supported by this raytracer
data Geometry = Sphere Coord3 Float
              | Plane UnitVec3 Float
  deriving (Eq, Show)

-- |Implementation of intersect for Sphere and Plane geometries
instance Intersectable Geometry where
  intersect raySegment sphere@(Sphere center radius) =
      if d >= 0 && t <= maxDepth
      then Just (Hit t ipoint inormal itangent ibitangent sphere)
      else Nothing
    where (RaySeg (Ray (rayOrigin, dir), maxDepth)) = raySegment
          t           = min s0 s1
          s0          = ((-bc) - sqD) / (2*ac)
          s1          = ((-bc) + sqD) / (2*ac)
          ac          = dot ndir ndir
          bc          = 2 * dot voffs ndir
          cc          = dot voffs voffs - (radius*radius)
          sqD         = sqrt d
          d           = (bc*bc) - (4*ac*cc)
          ndir        = normalized dir
          (P voffs)   = rayOrigin - center
          ipoint      = rayOrigin .+^ (t *^ normalized dir)
          inormal     = normalize3 $ ipoint .-. center
          itangent    = mkTangent inormal
          ibitangent  = normalize <$> liftA2 cross inormal itangent

  intersect raySegment plane@(Plane normal d) =
      if t >= 0 && t <= maxDepth  
      then Just (Hit t point' normal itangent ibitangent plane)
      else Nothing
    where (RaySeg (Ray (rayOrigin, dir), maxDepth)) = raySegment
          (P p0)      = rayOrigin
          t           = (-(dot p0 nnormal + d)) /
            dot ndir nnormal
          ndir        = normalized dir
          nnormal     = normalized normal
          point'      = rayOrigin .+^ (t *^ ndir)
          itangent    = mkTangent normal
          ibitangent  = normalize <$> liftA2 cross normal itangent
