module OpenSolid.Core.Vector3d
  ( zero
  , xComponent
  , yComponent
  , zComponent
  , components
  , squaredLength
  , length
  , normalized
  , direction
  , perpendicularVector
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
import OpenSolid.Core.Matrix3x2 as Matrix3x2


zero: Vector3d
zero =
  Vector3d 0 0 0


xComponent: Vector3d -> Float
xComponent (Vector3d x y z) =
  x


yComponent: Vector3d -> Float
yComponent (Vector3d x y z) =
  y


zComponent: Vector3d -> Float
zComponent (Vector3d x y z) =
  z


components: Vector3d -> (Float, Float, Float)
components (Vector3d x y z) =
  (x, y, z)


squaredLength: Vector3d -> Float
squaredLength (Vector3d x y z) =
  x * x + y * y + z * z


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
direction vector =
  Direction3d (normalized vector)


perpendicularVector: Vector3d -> Vector3d
perpendicularVector (Vector3d x y z) =
  let
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
transformedBy (Transformation3d transformVector transformPoint) =
  transformVector


projectedOntoAxis: Axis3d -> Vector3d -> Vector3d
projectedOntoAxis (Axis3d originPoint (Direction3d directionVector)) vector =
  times (dot directionVector vector) directionVector


projectedOntoPlane: Plane3d -> Vector3d -> Vector3d
projectedOntoPlane (Plane3d originPoint xDirection yDirection (Direction3d normalVector)) vector =
  let
    normalComponent = dot normalVector vector
  in
    minus (times normalComponent normalVector) vector


projectedIntoPlane: Plane3d -> Vector3d -> Vector2d
projectedIntoPlane (Plane3d originPoint xDirection yDirection normalDirection) vector =
  Matrix3x2.dotProduct xDirection yDirection vector


negated: Vector3d -> Vector3d
negated (Vector3d x y z) =
  Vector3d (-x) (-y) (-z)


plus: Vector3d -> Vector3d -> Vector3d
plus (Vector3d otherX otherY otherZ) (Vector3d x y z) =
  Vector3d (x + otherX) (y + otherY) (z + otherZ)


minus: Vector3d -> Vector3d -> Vector3d
minus (Vector3d otherX otherY otherZ) (Vector3d x y z) =
  Vector3d (x - otherX) (y - otherY) (z - otherZ)


times: Float -> Vector3d -> Vector3d
times scale (Vector3d x y z) =
  Vector3d (scale * x) (scale * y) (scale * z)


dot: Vector3d -> Vector3d -> Float
dot (Vector3d otherX otherY otherZ) (Vector3d x y z) =
  x * otherX + y * otherY + z * otherZ


cross: Vector3d -> Vector3d -> Vector3d
cross (Vector3d otherX otherY otherZ) (Vector3d x y z) =
  Vector3d
    (y * otherZ - z * otherY)
    (z * otherX - x * otherZ)
    (x * otherY - y * otherX)
