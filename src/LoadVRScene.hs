module LoadVRScene
  ( loadVRScene
  , vrsCamera
  ) where

import Scene
import Light
import Camera
import Numeric.Units.Dimensional.Prelude( (*~), lumen )
import Linear
import Math
import Geometry
import Parser (readVRScene)
import Types.VRScene (VRScene(..))
import Types.Node (nodeGeometry)
import Types.GeomStaticMesh (GeomStaticMesh(..))
import Data.HashMap.Lazy as HashMap ((!), elems)
import Data.Array.IArray as Array (Array, (!))


paths =
  [ "/home/me/Programming/chsgrp/vrscenes/metronome_12K_instanced.vrscene"
  , "/home/me/Programming/chsgrp/vrscenes/metronome_12K.vrscene"
  , "/home/me/Programming/chsgrp/vrscenes/sodaCan_160K.vrscene"
  , "/home/me/Programming/chsgrp/vrscenes/topHat_800K.vrscene"
  ] 

loadVRScene :: Int -> IO Scene
loadVRScene n = do
  let light    = RectLight (p3d 0 85 0, V3 40 0 0, V3 0 0 40, 450*~lumen)  -- 40W incacestent bulb
      settings = Settings { rsLightSamplesCount = 5, rsSecondaryGICount = 15, rsPathMaxDepth = 3 }
      
  (vertices, normals) <- loadNode (paths !! n)

  let entities = triangles vertices normals

  pure $ Scene entities light zeroLightIntensity settings

loadNode :: FilePath -> IO ([Double], [Double])
loadNode path = do
  (VRScene nodes meshes) <- readVRScene path

  -- Currenty working with only one node and one geometry
  let mesh = head . elems $
        fmap (\node -> meshes HashMap.! nodeGeometry node) nodes
      
      vertices = extractVertices mesh
      normals  = extractNormals mesh

  pure (vertices, normals)

extractVertices, extractNormals :: GeomStaticMesh -> [Double]

extractVertices (GeomStaticMesh _ vertices faces _ _ _ _ _ _ _) = 
  extract vertices faces
extractNormals (GeomStaticMesh _ _ _ normals faceNormals _ _ _ _ _) = 
  extract normals faceNormals

extract :: Array Int (a, a, a) -> [Int] -> [a]
extract arr (i:is) = ix : iy : iz : extract arr is
  where (ix, iy, iz) = arr Array.! i
extract _ _ = []

triangles :: [Double] -> [Double] -> [Entity]
triangles (ax:ay:az:bx:by:bz:cx:cy:cz:vs) (nx:ny:nz:ns) =
  Entity (Triangle (p3d ax ay (-az)) (p3d bx by (-bz)) (p3d cx cy (-cz)) (unitV3d nx ny (-nz)))
         (mkDiffuse 0.5 0.5 0.5) : triangles vs ns
triangles _ _ = []

vrsCamera :: PinholeCamera
vrsCamera = PinholeCamera sensor camPos' camDir' camUp' camFocal
  where sensor   = Sensor (192, 148, camSize, 3e+1)
        camPos'  = p3d 0 0 (-10)
        camDir'  = unitV3d 0 0 (-1)
        camUp'   = unitV3d 0 (-1) 0
        camFocal = 1.0        -- 10mm
        camSize  = V2 3.6 2.4 -- 35mm
