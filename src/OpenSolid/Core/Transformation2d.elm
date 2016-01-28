module OpenSolid.Core.Transformation2d
  ( translationBy
  , rotationAbout
  , localizationTo
  , globalizationFrom
  , andThen
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Matrix2x2 as Matrix2x2


translationBy: Vector2d -> Transformation2d
translationBy (Vector2d vx vy) =
  let
    transformPoint (Point2d px py) = Point2d (px + vx) (py + vy)
  in
    Transformation2d identity transformPoint


rotateVector: Float -> Float -> Vector2d -> Vector2d
rotateVector sinAngle cosAngle (Vector2d x y) =
  Vector2d (x * cosAngle - y * sinAngle) (x * sinAngle + y * cosAngle)


rotatePoint: Point2d -> Float -> Float -> Point2d -> Point2d
rotatePoint (Point2d x0 y0) sinAngle cosAngle (Point2d px py) =
  let
    (Vector2d dx dy) = rotateVector sinAngle cosAngle (Vector2d (px - x0) (py - y0))
  in
    Point2d (x0 + dx) (y0 + dy)


rotationAbout: Point2d -> Float -> Transformation2d
rotationAbout point angle =
  let
    sinAngle = sin angle
    cosAngle = cos angle
  in
    Transformation2d (rotateVector sinAngle cosAngle) (rotatePoint point sinAngle cosAngle)


localizationTo: Frame2d -> Transformation2d
localizationTo frame =
  let
    transformVector = Matrix2x2.dotProduct frame.xDirection frame.yDirection
    (Point2d x0 y0) = frame.originPoint
    transformPoint (Point2d px py) =
      let
        (Vector2d x y) = transformVector (Vector2d (px - x0) (py - y0))
      in
        Point2d x y
  in
    Transformation2d transformVector transformPoint


globalizationFrom: Frame2d -> Transformation2d
globalizationFrom frame =
  let
    transformVector = Matrix2x2.product frame.xDirection frame.yDirection
    (Point2d x0 y0) = frame.originPoint
    transformPoint (Point2d px py) =
      let
        (Vector2d dx dy) = transformVector (Vector2d px py)
      in
        Point2d (x0 + dx) (y0 + dy)
  in
    Transformation2d transformVector transformPoint


andThen: Transformation2d -> Transformation2d -> Transformation2d
andThen second first =
  let
    transformVector = first.transformVector >> second.transformVector
    transformPoint = first.transformPoint >> second.transformPoint
  in
    Transformation2d transformVector transformPoint
