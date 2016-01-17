module OpenSolid.Core.Vector2d
  ( zero
  , components
  , squaredLength
  , length
  , normalized
  , direction
  , normalDirection
  , transformedBy
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
import OpenSolid.Core.Math2d as Math2d
import OpenSolid.Core.Math3d as Math3d
import OpenSolid.Core.Matrix3x2 as Matrix3x2


zero: Vector2d
zero =
  Vector2d 0 0


components: Vector2d -> (Float, Float)
components vector =
  (vector.x, vector.y)


squaredLength: Vector2d -> Float
squaredLength =
  Math2d.squaredNorm


length: Vector2d -> Float
length =
  Math2d.norm


normalized: Vector2d -> Vector2d
normalized =
  Math2d.normalized


direction: Vector2d -> Direction2d
direction =
  normalized


normalDirection: Vector2d -> Direction2d
normalDirection vector =
  normalized (Math2d.perpendicular vector)


transformedBy: Transformation2d -> Vector2d -> Vector2d
transformedBy =
  fst


projectedOntoAxis: Axis2d -> Vector2d -> Vector2d
projectedOntoAxis axis =
  Math2d.projectedOnto axis.direction


placedOntoPlane: Plane3d -> Vector2d -> Vector3d
placedOntoPlane plane =
  Matrix3x2.product plane.xDirection plane.yDirection


negated: Vector2d -> Vector2d
negated =
  Math2d.negated


plus: Vector2d -> Vector2d -> Vector2d
plus =
  Math2d.plus


minus: Vector2d -> Vector2d -> Vector2d
minus =
  Math2d.minus


times: Float -> Vector2d -> Vector2d
times =
  Math2d.times


dot: Vector2d -> Vector2d -> Float
dot =
  Math2d.dot


cross: Vector2d -> Vector2d -> Float
cross =
  Math2d.cross
