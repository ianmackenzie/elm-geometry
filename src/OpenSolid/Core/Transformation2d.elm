module OpenSolid.Core.Transformation2d
  ( translationBy
  , rotationAbout
  , localizationTo
  , globalizationFrom
  , sequence
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Matrix2x2 as Matrix2x2


translationBy: Vector2d -> Transformation2d
translationBy (Vector2d vx vy) =
  (identity, (\(Point2d px py) -> Point2d (px + vx) (py + vy)))


rotateVector: Float -> Float -> Vector2d -> Vector2d
rotateVector cosAngle sinAngle (Vector2d vx vy) =
  Vector2d (vx * cosAngle - vy * sinAngle) (vy * cosAngle + vx * sinAngle)


rotatePoint: Point2d -> Float -> Float -> Point2d -> Point2d
rotatePoint (Point2d x0 y0) cosAngle sinAngle (Point2d px py) =
  let
    (Vector2d vx vy) = rotateVector cosAngle sinAngle (Vector2d (px - x0) (py - y0))
  in
    Point2d (x0 + vx) (y0 + vy)


rotationAbout: Point2d -> Float -> Transformation2d
rotationAbout centerPoint angle =
  let
    cosAngle = cos angle
    sinAngle = sin angle
  in
    (rotateVector cosAngle sinAngle, rotatePoint centerPoint cosAngle sinAngle)


localizationTo: Frame2d -> Transformation2d
localizationTo frame =
  let
    transformVector = Matrix2x2.dotProduct frame.xDirection frame.yDirection
    transformPoint (Point2d px py) =
      let
        (Point2d x0 y0) = frame.originPoint
        (Vector2d x y) = transformVector (Vector2d (px - x0) (py - y0))
      in
        Point2d x y
  in
    (transformVector, transformPoint)


globalizationFrom: Frame2d -> Transformation2d
globalizationFrom frame =
  let
    transformVector = Matrix2x2.product frame.xDirection frame.yDirection
    transformPoint (Point2d px py) =
      let
        (Vector2d vx vy) = transformVector (Vector2d px py)
        (Point2d x0 y0) = frame.originPoint
      in
        Point2d (x0 + vx) (y0 + vy)
  in
    (transformVector, transformPoint)


sequence: List Transformation2d -> Transformation2d
sequence transformations =
  let
    (vectorTransformations, pointTransformations) = List.unzip transformations
    transformVector vector =
      List.foldl (<|) vector vectorTransformations
    transformPoint point =
      List.foldl (<|) point pointTransformations
  in
    (transformVector, transformPoint)
