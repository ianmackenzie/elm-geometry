module OpenSolid.Core.Vector3d
  ( zero
  , components
  , squaredLength
  , length
  , normalized
  , direction
  , normalDirection
  , transformedBy
  , projectedOntoAxis
  , projectedOntoPlane
  , projectedIntoPlane
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


zero: Vector3d
zero =
  Vector3d 0 0 0


components: Vector3d -> (Float, Float, Float)
components vector =
  (vector.x, vector.y, vector.z)


squaredLength: Vector3d -> Float
squaredLength =
  Math3d.squaredNorm


length: Vector3d -> Float
length =
  Math3d.norm


normalized: Vector3d -> Vector3d
normalized =
  Math3d.normalized


direction: Vector3d -> Direction3d
direction =
  normalized


normalDirection: Vector3d -> Direction3d
normalDirection =
  Math3d.perpendicularDirection


transformedBy: Transformation3d -> Vector3d -> Vector3d
transformedBy =
  fst


projectedOntoAxis: Axis3d -> Vector3d -> Vector3d
projectedOntoAxis axis vector =
  Math3d.projectedOnto axis.direction vector


projectedOntoPlane: Plane3d -> Vector3d -> Vector3d
projectedOntoPlane plane vector =
  let
    normalComponent = dot plane.normalDirection vector
  in
    minus (times normalComponent plane.normalDirection) vector


projectedIntoPlane: Plane3d -> Vector3d -> Vector2d
projectedIntoPlane plane vector =
  Matrix3x2.dotProduct plane.xDirection plane.yDirection vector


negated: Vector3d -> Vector3d
negated =
  Math3d.negated


plus: Vector3d -> Vector3d -> Vector3d
plus =
  Math3d.plus


minus: Vector3d -> Vector3d -> Vector3d
minus =
  Math3d.minus


times: Float -> Vector3d -> Vector3d
times =
  Math3d.times


dot: Vector3d -> Vector3d -> Float
dot =
  Math3d.dot


cross: Vector3d -> Vector3d -> Vector3d
cross =
  Math3d.cross
