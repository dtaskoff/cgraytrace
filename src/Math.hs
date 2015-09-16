module Math( Coord3(..), Normal, normalized, normalize3, Ray(..) ) where

import Linear.V3
import Linear.Affine
import Linear.Metric (normalize)
import Linear.Epsilon

type Vec3   = V3 Float
type Coord3 = Point V3 Float

newtype Ray = Ray (Coord3, Normal)        -- position & direction

-- Track normalized vectors type-safe
newtype Normal = Normal Vec3

normalize3 :: Vec3 -> Normal
normalize3 = Normal . normalize

normalized :: Normal -> Vec3
normalized (Normal v) = v