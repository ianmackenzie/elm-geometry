module OpenSolid.Core.Vector2d
  ( zero
  , xComponent
  , yComponent
  , components
  , squaredLength
  , length
  , normalized
  , direction
  , perpendicularVector
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
import OpenSolid.Core.Matrix3x2 as Matrix3x2


zero: Vector2d
zero =
  Vector2d 0 0


xComponent: Vector2d -> Float
xComponent (Vector2d x y) =
  x


yComponent: Vector2d -> Float
yComponent (Vector2d x y) =
  y


components: Vector2d -> (Float, Float)
components (Vector2d x y) =
  (x, y)


squaredLength: Vector2d -> Float
squaredLength (Vector2d x y) =
  x * x + y * y


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
direction vector =
  Direction2d (normalized vector)


perpendicularVector: Vector2d -> Vector2d
perpendicularVector (Vector2d x y) =
  Vector2d (-y) x


normalDirection: Vector2d -> Direction2d
normalDirection =
  perpendicularVector >> direction


transformedBy: Transformation2d -> Vector2d -> Vector2d
transformedBy (Transformation2d transformVector transformPoint) =
  transformVector


projectedOntoAxis: Axis2d -> Vector2d -> Vector2d
projectedOntoAxis (Axis2d originPoint (Direction2d directionVector)) vector =
  times (dot directionVector vector) directionVector


placedOntoPlane: Plane3d -> Vector2d -> Vector3d
placedOntoPlane (Plane3d originPoint xDirection yDirection normalDirection) =
  Matrix3x2.product xDirection yDirection


negated: Vector2d -> Vector2d
negated (Vector2d x y) =
  Vector2d (-x) (-y)


plus: Vector2d -> Vector2d -> Vector2d
plus (Vector2d otherX otherY) (Vector2d x y) =
  Vector2d (x + otherX) (y + otherY)


minus: Vector2d -> Vector2d -> Vector2d
minus (Vector2d otherX otherY) (Vector2d x y) =
  Vector2d (x - otherX) (y - otherY)


times: Float -> Vector2d -> Vector2d
times scale (Vector2d x y) =
  Vector2d (scale * x) (scale * y)


dot: Vector2d -> Vector2d -> Float
dot (Vector2d otherX otherY) (Vector2d x y) =
  x * otherX + y * otherY


cross: Vector2d -> Vector2d -> Float
cross (Vector2d otherX otherY) (Vector2d x y) =
  x * otherY - y * otherX
