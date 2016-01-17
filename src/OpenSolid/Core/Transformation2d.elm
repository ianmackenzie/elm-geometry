module OpenSolid.Core.Transformation2d
  ( translationBy
  , rotationAbout
  , localizationTo
  , globalizationFrom
  , andThen
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Math2d as Math2d
import OpenSolid.Core.Matrix2x2 as Matrix2x2


translationBy: Vector2d -> Transformation2d
translationBy vector =
  ( identity
  , \point -> Math2d.plus vector point
  )


rotateVector: Float -> Float -> Vector2d -> Vector2d
rotateVector sinAngle cosAngle vector =
  let
    x = vector.x * cosAngle - vector.y * sinAngle
    y = vector.x * sinAngle + vector.y * cosAngle
  in
    Vector2d x y


rotatePoint: Point2d -> Float -> Float -> Point2d -> Point2d
rotatePoint originPoint sinAngle cosAngle point =
  let
    vector = Math2d.minus originPoint point
    rotatedVector = rotateVector sinAngle cosAngle vector
  in
    Math2d.plus rotatedVector originPoint


rotationAbout: Point2d -> Float -> Transformation2d
rotationAbout point angle =
  let
    sinAngle = sin angle
    cosAngle = cos angle
  in
    ( rotateVector sinAngle cosAngle
    , rotatePoint point sinAngle cosAngle
    )


localizationTo: Frame2d -> Transformation2d
localizationTo frame =
  let
    transformVector = Matrix2x2.dotProduct frame.xDirection frame.yDirection
  in
    ( transformVector
    , Math2d.minus frame.originPoint >> transformVector
    )


globalizationFrom: Frame2d -> Transformation2d
globalizationFrom frame =
  let
    transformVector = Matrix2x2.product frame.xDirection frame.yDirection
  in
    ( transformVector
    , transformVector >> Math2d.plus frame.originPoint
    )


andThen: Transformation2d -> Transformation2d -> Transformation2d
andThen second first =
  ( fst first >> fst second
  , snd first >> snd second
  )
