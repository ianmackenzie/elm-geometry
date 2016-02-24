module OpenSolid.Core.Vector3d
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
import OpenSolid.Core.Components3d as Components3d
import OpenSolid.Core.Matrix3x2 as Matrix3x2


zero: Vector3d
zero =
  Vector3d 0 0 0


toTuple: Vector3d -> (Float, Float, Float)
toTuple =
  Components3d.toTuple


componentIn: Direction3d -> Vector3d -> Float
componentIn =
  dot


squaredLength: Vector3d -> Float
squaredLength vector =
  vector.x * vector.x + vector.y * vector.y + vector.z * vector.z


length: Vector3d -> Float
length =
  squaredLength >> sqrt


normalized: Vector3d -> Vector3d
normalized vector =
  let
    vectorSquaredLength = squaredLength vector
  in
    if vectorSquaredLength == 0 then
      zero
    else
      times (1 / (sqrt vectorSquaredLength)) vector


direction: Vector3d -> Direction3d
direction =
  normalized


perpendicularVector: Vector3d -> Vector3d
perpendicularVector vector =
  let
    x = vector.x
    y = vector.y
    z = vector.z
    absX = abs x
    absY = abs y
    absZ = abs z
  in
    if absX <= absY then
      if absX <= absZ then
        Vector3d 0 (-z) y
      else
        Vector3d (-y) x 0
    else
      if absY <= absZ then
        Vector3d z 0 (-x)
      else
        Vector3d (-y) x 0


normalDirection: Vector3d -> Direction3d
normalDirection =
  perpendicularVector >> direction


transformedBy: Transformation3d -> Vector3d -> Vector3d
transformedBy transformation =
  transformation.transformVector


projectedOnto: Direction3d -> Vector3d -> Vector3d
projectedOnto direction vector =
    times (componentIn direction vector) direction


projectedOntoAxis: Axis3d -> Vector3d -> Vector3d
projectedOntoAxis axis =
  projectedOnto axis.direction


projectedOntoPlane: Plane3d -> Vector3d -> Vector3d
projectedOntoPlane plane vector =
  minus (projectedOnto plane.normalDirection vector) vector


projectedIntoPlane: Plane3d -> Vector3d -> Vector2d
projectedIntoPlane plane vector =
  Matrix3x2.dotProduct plane.xDirection plane.yDirection vector


negated: Vector3d -> Vector3d
negated =
  Components3d.negated


plus: Vector3d -> Vector3d -> Vector3d
plus =
  Components3d.plus


minus: Vector3d -> Vector3d -> Vector3d
minus =
  Components3d.minus


times: Float -> Vector3d -> Vector3d
times =
  Components3d.times


dot: Vector3d -> Vector3d -> Float
dot other vector =
  vector.x * other.x + vector.y * other.y + vector.z * other.z


cross: Vector3d -> Vector3d -> Vector3d
cross other vector =
  let
    x = vector.y * other.z - vector.z * other.y
    y = vector.z * other.x - vector.x * other.z
    z = vector.x * other.y - vector.y * other.x
  in
    Vector3d x y z
