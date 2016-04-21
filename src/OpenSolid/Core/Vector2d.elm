module OpenSolid.Core.Vector2d
  ( zero
  , fromTuple
  , toTuple
  , equals
  , componentIn
  , squaredLength
  , length
  , normalized
  , direction
  , perpendicularVector
  , normalDirection
  , transformedBy
  , projectedOnto
  , projectedOntoAxis
  , placedOntoPlane
  , negated
  , plus
  , minus
  , times
  , dot
  , cross
  ) where


import Maybe exposing (..)
import OpenSolid.Core exposing (..)
import OpenSolid.Core.Matrix3x2 as Matrix3x2


toVector2d: Direction2d -> Vector2d
toVector2d (Direction2d x y) =
  Vector2d x y


toDirection2d: Vector2d -> Direction2d
toDirection2d (Vector2d x y) =
  Direction2d x y


zero: Vector2d
zero =
  Vector2d 0 0


fromTuple: (Float, Float) -> Vector2d
fromTuple (x, y) =
  Vector2d x y


toTuple: Vector2d -> (Float, Float)
toTuple (Vector2d x y) =
  (x, y)


equals: Vector2d -> Vector2d -> Bool
equals (Vector2d x2 y2) (Vector2d x1 y1) =
  x1 == x2 && y1 == y2


componentIn: Direction2d -> Vector2d -> Float
componentIn (Direction2d x2 y2) (Vector2d x1 y1) =
  x1 * x2 + y1 * y2


squaredLength: Vector2d -> Float
squaredLength (Vector2d x y) =
  x * x + y * y


length: Vector2d -> Float
length =
  squaredLength >> sqrt


normalized: Vector2d -> Maybe Vector2d
normalized vector =
  if equals zero vector then Nothing else Just (times (1 / length vector) vector)


direction: Vector2d -> Maybe Direction2d
direction =
  normalized >> Maybe.map toDirection2d


perpendicularVector: Vector2d -> Vector2d
perpendicularVector (Vector2d x y) =
  Vector2d (-y) x


normalDirection: Vector2d -> Maybe Direction2d
normalDirection =
  perpendicularVector >> direction


transformedBy: Transformation2d -> Vector2d -> Vector2d
transformedBy (transformVector, _) =
  transformVector


projectedOnto: Direction2d -> Vector2d -> Vector2d
projectedOnto direction vector =
  times (componentIn direction vector) (toVector2d direction)


projectedOntoAxis: Axis2d -> Vector2d -> Vector2d
projectedOntoAxis axis =
  projectedOnto axis.direction


placedOntoPlane: Plane3d -> Vector2d -> Vector3d
placedOntoPlane plane =
  Matrix3x2.product plane.xDirection plane.yDirection


negated: Vector2d -> Vector2d
negated (Vector2d x y) =
  Vector2d (-x) (-y)


plus: Vector2d -> Vector2d -> Vector2d
plus (Vector2d x2 y2) (Vector2d x1 y1) =
  Vector2d (x1 + x2) (y1 + y2)


minus: Vector2d -> Vector2d -> Vector2d
minus (Vector2d x2 y2) (Vector2d x1 y1) =
  Vector2d (x1 - x2) (y1 - y2)


times: Float -> Vector2d -> Vector2d
times scale (Vector2d x y) =
  Vector2d (x * scale) (y * scale)


dot: Vector2d -> Vector2d -> Float
dot (Vector2d x2 y2) (Vector2d x1 y1) =
  x1 * x2 + y1 * y2


cross: Vector2d -> Vector2d -> Float
cross (Vector2d x2 y2) (Vector2d x1 y1) =
  x1 * y2 - y1 * x2
