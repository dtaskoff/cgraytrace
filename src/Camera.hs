module Camera where

import Math
import Linear
import Linear.Affine


newtype UnitSpace = US (V2 Double)

newtype Sensor = Sensor (Int, Int, V2 Double, Double)
-- ^ width, height, physical size, exposure

-- | Interface for all cameras with two needed functions
class Camera cam where
  cameraRay    :: cam -> UnitSpace -> RaySegment
  cameraSensor :: cam -> Sensor

-- | Our cameras
data PinholeCamera = PinholeCamera
  { phcamSensor      :: Sensor
  , phcamPos         :: P3d
  , phcamDir         :: UnitV3d
  , phcamUp          :: UnitV3d
  , phcamFocalLength :: Double
  }

data OrthoCamera = OrthoCamera
  { orthoSensor :: Sensor
  , orthoPos    :: P3d
  , orthoDir    :: UnitV3d
  , orthoUp     :: UnitV3d
  }

-- | Implementation of Ortho camera
instance Camera OrthoCamera where
  cameraRay (OrthoCamera sensor pos dir up) (US imagePos) =
      RaySeg (Ray (start, dir), farthestDistance)
    where Sensor (_,_,sensorSize,_)  = sensor
          start         = pos .+^ (vpos3 *! view)
          vpos3         = V3 (x*aspect) y 0.0
          view          = V3 xaxis yaxis zaxis

          (V2 x y)      = (imagePos - V2 0.5 0.5) * sensorSize
          aspect        = sensorAspect sensor

          xaxis         = normalize $ cross (normalized up) zaxis
          yaxis         = cross zaxis xaxis
          zaxis         = normalized dir

  cameraSensor = orthoSensor

-- | Implementation of Pinhole camera
instance Camera PinholeCamera where
  cameraRay (PinholeCamera sensor pos dir up focalLength) (US imagePos) =
      RaySeg (Ray (pos, vunitV3d proj), farthestDistance)
    where Sensor (_,_,sensorSize,_) = sensor
          proj     = v *! view
          v        = d ^* focalLength - vpos3
          view     = V3 xaxis yaxis zaxis

          d        = normalized dir
          vpos3    = V3 (x*aspect) (-y) 0.0
          (V2 x y) = (imagePos - V2 0.5 0.5) * sensorSize
          aspect   = sensorAspect sensor

          xaxis    = normalize $ cross (normalized up) zaxis
          yaxis    = cross zaxis xaxis
          zaxis    = normalized dir

  cameraSensor = phcamSensor


---------------------------------------------------------------------
sensorAspect :: Sensor -> Double
sensorAspect (Sensor (w,h,V2 sw sy,_)) =
    fromIntegral w / fromIntegral h / sizeAspect
  where sizeAspect = sw / sy

toScreenSpace :: Sensor -> Int -> Int -> UnitSpace
toScreenSpace (Sensor (width, height, _, _)) x y = US $ V2 sx sy
  where sx = fromIntegral x / (fromIntegral width  - 1)
        sy = fromIntegral y / (fromIntegral height - 1)
