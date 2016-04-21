{-# LANGUAGE NoMonomorphismRestriction
           , QuasiQuotes
           , TemplateHaskell
           , TypeFamilies
           , OverloadedStrings
           , ViewPatterns
  #-}
module Main where

import Yesod
import Codec.Picture.Png
import Raytracer (raytrace)
import Scene
import System.Random.TF.Gen (seedTFGen)

import LoadVRScene (loadVRScene, vrsCamera)


-- Little RESTful HTTP web server to show us rendering result
data App = App
instance Yesod App

mkYesod "App" [parseRoutes| 
/              ImageR     GET
/res/#Int/#Int ImageSizeR GET
/tetra         TetraR     GET
/vrscene/#Int  VRSceneR   GET
|]

getImageSizeR :: MonadHandler m => Int -> Int -> m TypedContent
getImageSizeR width height = do
  let gen = seedTFGen (1,2,3,4)
      image = raytrace gen cornellScene $ cornellCamera width height

  sendResponse $ toTypedContent (typePng, toContent (encodePng image))


-- | HTTP GET at "host/" address that returns us a ray traced image
getImageR :: MonadHandler m => m TypedContent
getImageR = getImageSizeR 320 240

getTetraR :: MonadHandler m => m TypedContent
getTetraR = do
  let gen = seedTFGen (1,2,3,4)
      image = raytrace gen cornellScene2 $ cornellCamera 320 240

  sendResponse $ toTypedContent (typePng, toContent (encodePng image))

getVRSceneR :: MonadHandler m => Int -> m TypedContent
getVRSceneR n = do
  scene <- liftIO (loadVRScene n)
  let gen = seedTFGen (1,2,3,4)
      image = raytrace gen scene vrsCamera

  sendResponse $ toTypedContent (typePng, toContent (encodePng image))


main :: IO ()
main = warpEnv App
