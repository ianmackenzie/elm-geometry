module OpenSolid.Core.Transformation2d
  ( translationBy
  , rotationAbout
  , localizationTo
  , globalizationFrom
  , sequence
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Matrix2x2 as Matrix2x2
import OpenSolid.Core.Point2d as Point2d
import OpenSolid.Core.Vector2d as Vector2d


translationBy: Vector2d -> Transformation2d
translationBy vector =
  (identity, Point2d.plus vector)


rotationAbout: Point2d -> Float -> Transformation2d
rotationAbout centerPoint angle =
  let
    cosine = cos angle
    sine = sin angle
    rotateVector (Vector2d x y) = Vector2d (x * cosine - y * sine) (y * cosine + x * sine)
    rotatePoint = Point2d.vectorFrom centerPoint >> rotateVector >> Vector2d.addedTo centerPoint
  in
    (rotateVector, rotatePoint)


localizationTo: Frame2d -> Transformation2d
localizationTo frame =
  let
    localizeVector = Matrix2x2.dotProduct frame.xDirection frame.yDirection
    vectorToPoint (Vector2d x y) = Point2d x y
    localizePoint = Point2d.vectorFrom frame.originPoint >> localizeVector >> vectorToPoint
  in
    (localizeVector, localizePoint)


globalizationFrom: Frame2d -> Transformation2d
globalizationFrom frame =
  let
    globalizeVector = Matrix2x2.product frame.xDirection frame.yDirection
    pointToVector (Point2d x y) = Vector2d x y
    globalizePoint = pointToVector >> globalizeVector >> Vector2d.addedTo frame.originPoint
  in
    (globalizeVector, globalizePoint)


sequence: List Transformation2d -> Transformation2d
sequence transformations =
  let
    (vectorFunctions, pointFunctions) = List.unzip transformations
    chain functions argument = List.foldl (<|) argument functions
  in
    (chain vectorFunctions, chain pointFunctions)
