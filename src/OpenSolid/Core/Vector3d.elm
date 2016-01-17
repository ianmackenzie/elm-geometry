module OpenSolid.Core.Vector3d
  ( Vector3d
  , zero
  , components
  , squaredLength
  , length
  , normalized
  , direction
  , normalDirection
  , transformedBy
  , projectedOntoAxis
  , negated
  , plus
  , minus
  , times
  , dot
  , cross
  ) where


import OpenSolid.Core exposing (..)


zero: Vector3d
zero =
  Vector3d 0 0 0


components: Vector3d -> (Float, Float, Float)
components vector =
  (vector.x, vector.y, vector.z)


squaredLength: Vector3d -> Float
squaredLength vector =
  vector.x^2 + vector.y^2 + vector.z^2


length: Vector3d -> Float
length =
  squaredLength >> sqrt


normalized: Vector3d -> Vector3d
normalized vector =
  let
    length' = length vector
  in
    if length' == 0 then
      zero
    else
      times (1 / length') vector


direction: Vector3d -> Direction3d
direction =
  normalized


normalDirection: Vector3d -> Direction3d
normalDirection vector =
  if vector == zero then
    Direction3d 0 0
  else
    let
      absX = abs vector.x
      absY = abs vector.y
      absZ = abs vector.z
      other =
        if absX <= absY then
          if absX <= absZ then
            Vector3d 1 0 0
          else
            Vector3d 0 0 1
        else
          if absY <= absZ then
            Vector3d 0 1 0
          else
            Vector3d 0 0 1
    in
      direction (cross other)


transformedBy: Transformation3d -> Vector3d -> Vector3d
transformedBy =
  fst


projectedOntoAxis: Axis3d -> Vector3d -> Vector3d
projectedOntoAxis axis vector =
  times (dot axis.direction vector) axis.direction


negated: Vector3d -> Vector3d
negated vector =
  Vector3d (-vector.x) (-vector.y) (-vector.z)


plus: Vector3d -> Vector3d -> Vector3d
plus other vector =
  Vector3d (vector.x + other.x) (vector.y + other.y) (vector.z + other.z)


minus: Vector3d -> Vector3d -> Vector3d
minus other vector =
  Vector3d (vector.x - other.x) (vector.y - other.y) (vector.z - other.z)


times: Float -> Vector3d -> Vector3d
times scale vector =
  Vector3d (vector.x * scale) (vector.y * scale) (vector.z * scale)


dot: Vector3d -> Vector3d -> Float
dot other vector =
  vector.x * other.x + vector.y * other.y + vector.z * other.z


cross: Vector3d -> Vector3d -> Vector3d
cross other vector =
  Vector3d
    (vector.y * other.z - vector.z * other.y)
    (vector.z * other.x - vector.x * other.z)
    (vector.x * other.y - vector.y * other.x)
