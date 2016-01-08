module OpenSolid.Transformation2d
  ( Transformation2d
  , translationBy
  , rotationAbout
  , composition
  ) where


import OpenSolid.Direction2d as Direction2d exposing (Direction2d(Direction2d))
import OpenSolid.Vector2d as Vector2d exposing (Vector2d(Vector2d))
import OpenSolid.Point2d as Point2d exposing (Point2d(Point2d))


type alias Transformation2d =
  { ofDirection: Direction2d -> Direction2d
  , ofVector: Vector2d -> Vector2d
  , ofPoint: Point2d -> Point2d
  }


translationBy: Vector2d -> Transformation2d
translationBy vector =
  { ofDirection = \direction -> direction
  , ofVector = \vector -> vector
  , ofPoint = \point -> Point2d.sum point vector
  }


rotateDirection: Direction2d -> Float -> Float -> Direction2d
rotateDirection (Direction2d x y) sinAngle cosAngle =
  Direction2d (x * cosAngle - y * sinAngle) (x * sinAngle + y * cosAngle)


rotateVector: Vector2d -> Float -> Float -> Vector2d
rotateVector (Vector2d x y) sinAngle cosAngle =
  Vector2d (x * cosAngle - y * sinAngle) (x * sinAngle + y * cosAngle)


rotatePoint: Point2d -> Point2d -> Float -> Float -> Point2d
rotatePoint point originPoint sinAngle cosAngle =
  Point2d.sum originPoint (rotateVector (Point2d.difference point originPoint) sinAngle cosAngle)


rotationAbout: Point2d -> Float -> Transformation2d
rotationAbout point angle =
  let
    sinAngle = sin angle
    cosAngle = cos angle
  in
    { ofDirection = \direction -> rotateDirection direction sinAngle cosAngle
    , ofVector = \vector -> rotateVector vector sinAngle cosAngle
    , ofPoint = \point' -> rotatePoint point' point sinAngle cosAngle
    }


composition: Transformation2d -> Transformation2d -> Transformation2d
composition outer inner =
  { ofDirection = \direction -> outer.ofDirection (inner.ofDirection direction)
  , ofVector = \vector -> outer.ofVector (inner.ofVector vector)
  , ofPoint = \point -> outer.ofPoint (inner.ofPoint point)
  }
