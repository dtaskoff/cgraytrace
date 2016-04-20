module Scene where

import Numeric.Units.Dimensional.Prelude( (*~), lumen )
import Linear
import Linear.Affine (Point(P))
import Geometry
import Camera
import Light
import Material
import BRDF
import Math


-- Scene description
data Entity = Entity
  { enGeom     :: Geometry
  , enMaterial :: Material
  } deriving (Eq, Show)

data RenderSettings = Settings
  { rsLightSamplesCount :: Int
  , rsSecondaryGICount  :: Int
  , rsPathMaxDepth      :: Int
  }

data Scene = Scene
  { scEntities :: [Entity]
  , scLight    :: Light
  , scEnvLight :: LightIntensity
  , scSettings :: RenderSettings
  }

---------------------------------------------------------------------
-- Demo scenes
cornellScene :: Scene
cornellScene = Scene [leftWall, rightWall, bottomWall, backWall, topWall, sphere0]
    light1 zeroLightIntensity settings
  where leftWall   = Entity (Plane (unitV3d 1 0 0)    100)  (mkDiffuse 1.58 0 0)
        rightWall  = Entity (Plane (unitV3d (-1) 0 0) 100)  (mkDiffuse 0.0 0.0 0.28)
        bottomWall = Entity (Plane (unitV3d 0 1 0)    100)  (mkDiffuse 0.18 0.18 0)
        backWall   = Entity (Plane (unitV3d 0 0 (-1)) 100)  (mkDiffuse 0.18 0.18 0.18)
        topWall    = Entity (Plane (unitV3d 0 (-1)  0) 100) (mkDiffuse 0.18 0.18 0.18)
        sphere0    = Entity (Sphere (p3d  0 (-50) 0) 20)  (mkDiffuse 0.50 0.50 0.50)

        light1     = RectLight (p3d 0 85 0, V3 40 0 0, V3 0 0 40, 450*~lumen)  -- 40W incacestent bulb

        settings   = Settings { rsLightSamplesCount = 5, rsSecondaryGICount = 25, rsPathMaxDepth = 3 }

triangleNormal :: P3d -> P3d -> P3d -> UnitV3d
triangleNormal (P a) (P b) (P c) = vunitV3d $ n
  where n = v `cross` w
        v = b - a
        w = c - a

cornellScene2 :: Scene
cornellScene2 = Scene ([leftWall, rightWall, bottomWall, backWall, topWall] ++
    tetrahedron) light1 zeroLightIntensity settings
  where leftWall    = Entity (Plane (unitV3d 1 0 0)    100)  (mkDiffuse 1.58 0 0)
        rightWall   = Entity (Plane (unitV3d (-1) 0 0) 100)  (mkDiffuse 0.0 0.0 0.28)
        bottomWall  = Entity (Plane (unitV3d 0 1 0)    100)  (mkDiffuse 0.18 0.18 0)
        backWall    = Entity (Plane (unitV3d 0 0 (-1)) 100)  (mkDiffuse 0.18 0.18 0.18)
        topWall     = Entity (Plane (unitV3d 0 (-1)  0) 100) (mkDiffuse 0.18 0.18 0.18)
        tetrahedron = [ Entity (Triangle a b d $ triangleNormal a b d) (mkDiffuse 0.9 0.9 0.9)
                      , Entity (Triangle b c d $ triangleNormal b c d) (mkDiffuse 0.9 0.9 0.9)
                      , Entity (Triangle c a d $ triangleNormal c a d) (mkDiffuse 0.9 0.9 0.9)
                      , Entity (Triangle a c b $ triangleNormal a c b) (mkDiffuse 0.9 0.9 0.9)
                      ]
        a = p3d (-40) (-60) 20.0
        b = p3d 40    (-60) 20.0
        c = p3d 0     (-20) (-40.0)
        d = p3d 0     40 0

        light1     = RectLight (p3d 0 85 0, V3 40 0 0, V3 0 0 40, 450*~lumen)  -- 40W incacestent bulb

        settings   = Settings { rsLightSamplesCount = 5, rsSecondaryGICount = 25, rsPathMaxDepth = 3 }

cornellCamera :: Int -> Int -> PinholeCamera
cornellCamera width height = PinholeCamera sensor camPos' camDir' camUp' camFocal
  where sensor   = Sensor (width, height, camSize, 3e+1)
        --sensor   = Sensor (1280, 1024, camSize, 3e+1)
        camPos'  = p3d 0 0 (-80)
        camDir'  = unitV3d 0 0 1
        camUp'   = unitV3d 0 (-1) 0
        camFocal = 1.0        -- 10mm
        camSize  = V2 3.6 2.4 -- 35mm

demoScene :: Scene
demoScene = Scene [sphere0, sphere1, sphere2, plane0]
    light0 zeroLightIntensity settings
  where sphere0 = Entity (Sphere (p3d 0 0 200) 20)         (mkDiffuse 0.98 0 0)
        sphere1 = Entity (Sphere (p3d 5 35 200) 25)        (mkDiffuse 0 0.98 0)
        sphere2 = Entity (Sphere (p3d (-25) 20 180) 10)    (mkDiffuse 0 0.98 0.98)
        plane0  = Entity (Plane (unitV3d 0 0.5 (-0.5)) 150) (mkDiffuse 0.5 0.5 0.5)
        light0  = OmniLight (p3d (-40) 80 0, 100 *~lumen)
        settings = Settings { rsLightSamplesCount = 1, rsSecondaryGICount = 20, rsPathMaxDepth = 3 }

demoCamera :: PinholeCamera
demoCamera = demoCamera1

camPos :: P3d
camPos = p3d 0 0 0

camDir, camUp :: UnitV3d
camDir = unitV3d 0 0 1
camUp  = unitV3d 0 (-1) 0

demoCamera0 :: OrthoCamera
demoCamera0 = OrthoCamera sensor camPos camDir camUp
  where sensor   = Sensor (160, 160, camSize, 0.01)
        camSize  = V2 100 100

demoCamera1 :: PinholeCamera
demoCamera1 = PinholeCamera sensor camPos camDir camUp camFocal
  where sensor   = Sensor (360, 240, camSize, 0.01)
        camFocal = 4.0        -- 40mm
        camSize  = V2 3.6 2.4 -- 35mm

---------------------------------------------------------------------
liftIntersection :: Entity -> Intersection Geometry -> Intersection Entity
liftIntersection (Entity _ mat) (Hit d p n t bt g) = Hit d p n t bt (Entity g mat)

instance Intersectable Entity where
  intersect ray entity@(Entity geom _) = liftIntersection entity <$> intersect ray geom

mkDiffuse :: Double -> Double -> Double -> Material
mkDiffuse r g b = Mat . Diffuse $ transfer r g b
