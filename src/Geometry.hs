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
              | Triangle Coord3 Coord3 Coord3 UnitVec3
  deriving (Eq, Show)

-- |Implementation of intersect for Sphere and Plane geometries
instance Intersectable Geometry where
  intersect raySegment sphere@(Sphere center radius) =
      if d >= 0 && t <= maxDepth
      then Just (Hit t ipoint inormal itangent ibitangent sphere)
      else Nothing
    where (RaySeg (Ray (rayOrigin, dir), maxDepth)) = raySegment
          t          = min s0 s1
          s0         = ((-bc) - sqD) / (2*ac)
          s1         = ((-bc) + sqD) / (2*ac)
          ac         = dot ndir ndir
          bc         = 2 * dot voffs ndir
          cc         = dot voffs voffs - (radius*radius)
          sqD        = sqrt d
          d          = (bc*bc) - (4*ac*cc)
          ndir       = normalized dir
          (P voffs)  = rayOrigin - center
          ipoint     = rayOrigin .+^ (t *^ normalized dir)
          inormal    = normalize3 $ ipoint .-. center
          itangent   = mkTangent inormal
          ibitangent = normalize <$> liftA2 cross inormal itangent

  intersect raySegment plane@(Plane normal d) =
      if t >= 0 && t <= maxDepth  
      then Just (Hit t point' normal itangent ibitangent plane)
      else Nothing
    where (RaySeg (Ray (rayOrigin, dir), maxDepth)) = raySegment
          t          = negate (dot p0 nnormal + d) / dot ndir nnormal
          (P p0)     = rayOrigin
          nnormal    = normalized normal
          ndir       = normalized dir
          point'     = rayOrigin .+^ (t *^ ndir)
          itangent   = mkTangent normal
          ibitangent = normalize <$> liftA2 cross normal itangent

  intersect raySegment triangle@(Triangle (P a) (P b) (P c) normal) =
      if not parallel && t >= 0 && t <= maxDepth && inTriangle
      then Just (Hit t point' normal itangent ibitangent triangle)
      else Nothing
    where (RaySeg (Ray (rayOrigin, dir), maxDepth)) = raySegment
          parallel   = dot nnormal ndir == 0
          nnormal    = normalized normal
          ndir       = normalized dir
          t          = negate (dot p0 nnormal + d) / dot ndir nnormal
          (P p0)     = rayOrigin
          d          = negate $ dot nnormal a
          point'     = rayOrigin .+^ (t *^ ndir)
          itangent   = mkTangent normal
          ibitangent = normalize <$> liftA2 cross normal itangent
          inTriangle = dot nnormal v0 >= 0 && dot nnormal v1 >= 0 && dot nnormal v2 >= 0
          (v0,v1,v2) = (cross e0 c0, cross e1 c1, cross e2 c2)
          (e0,e1,e2) = (b - a, c - b, a - c)
          (c0,c1,c2) = (point - a, point - b, point - c)
          (P point)  = point'
