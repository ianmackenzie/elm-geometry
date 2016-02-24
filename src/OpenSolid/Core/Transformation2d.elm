module OpenSolid.Core.Transformation2d
  ( translationBy
  , rotationAbout
  , localizationTo
  , globalizationFrom
  , andThen
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Components2d as Components2d
import OpenSolid.Core.Matrix2x2 as Matrix2x2


translationBy: Vector2d -> Transformation2d
translationBy vector =
  Transformation2d identity (Components2d.plus vector)


rotateVector: Float -> Float -> Vector2d -> Vector2d
rotateVector sinAngle cosAngle vector =
  Vector2d (vector.x * cosAngle - vector.y * sinAngle) (vector.x * sinAngle + vector.y * cosAngle)


rotatePoint: Point2d -> Float -> Float -> Point2d -> Point2d
rotatePoint centerPoint sinAngle cosAngle point =
  let
    rotatedVector = rotateVector sinAngle cosAngle (Components2d.minus centerPoint point)
  in
    Components2d.plus rotatedVector centerPoint


rotationAbout: Point2d -> Float -> Transformation2d
rotationAbout centerPoint angle =
  let
    sinAngle = sin angle
    cosAngle = cos angle
  in
    Transformation2d (rotateVector sinAngle cosAngle) (rotatePoint centerPoint sinAngle cosAngle)


localizationTo: Frame2d -> Transformation2d
localizationTo frame =
  let
    transformVector = Matrix2x2.dotProduct frame.xDirection frame.yDirection
    transformPoint = Components2d.minus frame.originPoint >> transformVector
  in
    Transformation2d transformVector transformPoint


globalizationFrom: Frame2d -> Transformation2d
globalizationFrom frame =
  let
    transformVector = Matrix2x2.product frame.xDirection frame.yDirection
    transformPoint = transformVector >> Components2d.plus frame.originPoint
  in
    Transformation2d transformVector transformPoint


andThen: Transformation2d -> Transformation2d -> Transformation2d
andThen second first =
  let
    transformVector = first.transformVector >> second.transformVector
    transformPoint = first.transformPoint >> second.transformPoint
  in
    Transformation2d transformVector transformPoint
