module OpenSolid.Core.Vector2d
  ( zero
  , toTuple
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


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Components2d as Components2d
import OpenSolid.Core.Matrix3x2 as Matrix3x2


zero: Vector2d
zero =
  Vector2d 0 0


toTuple: Vector2d -> (Float, Float)
toTuple =
  Components2d.toTuple


componentIn: Direction2d -> Vector2d -> Float
componentIn =
  dot


squaredLength: Vector2d -> Float
squaredLength vector =
  vector.x * vector.x + vector.y * vector.y


length: Vector2d -> Float
length =
  squaredLength >> sqrt


normalized: Vector2d -> Vector2d
normalized vector =
  let
    vectorSquaredLength = squaredLength vector
  in
    if vectorSquaredLength == 0 then
      zero
    else
      times (1 / (sqrt vectorSquaredLength)) vector


direction: Vector2d -> Direction2d
direction =
  normalized


perpendicularVector: Vector2d -> Vector2d
perpendicularVector vector =
  Vector2d (-vector.y) vector.x


normalDirection: Vector2d -> Direction2d
normalDirection =
  perpendicularVector >> direction


transformedBy: Transformation2d -> Vector2d -> Vector2d
transformedBy transformation =
  transformation.transformVector


projectedOnto: Direction2d -> Vector2d -> Vector2d
projectedOnto direction vector =
    times (componentIn direction vector) direction


projectedOntoAxis: Axis2d -> Vector2d -> Vector2d
projectedOntoAxis axis =
  projectedOnto axis.direction


placedOntoPlane: Plane3d -> Vector2d -> Vector3d
placedOntoPlane plane =
  Matrix3x2.product plane.xDirection plane.yDirection


negated: Vector2d -> Vector2d
negated =
  Components2d.negated


plus: Vector2d -> Vector2d -> Vector2d
plus =
  Components2d.plus


minus: Vector2d -> Vector2d -> Vector2d
minus =
  Components2d.minus

times: Float -> Vector2d -> Vector2d
times =
  Components2d.times


dot: Vector2d -> Vector2d -> Float
dot other vector =
  vector.x * other.x + vector.y * other.y


cross: Vector2d -> Vector2d -> Float
cross other vector =
  vector.x * other.y - vector.y * other.x
