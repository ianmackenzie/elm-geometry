module OpenSolid.Core.Vector3d
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
  , projectionIn
  , projectedOnto
  , projectedInto
  , negated
  , plus
  , minus
  , times
  , addedTo
  , subtractedFrom
  , dot
  , cross
  ) where


import Maybe exposing (..)
import OpenSolid.Core exposing (..)
import OpenSolid.Core.Matrix3x2 as Matrix3x2


toVector: Direction3d -> Vector3d
toVector (Direction3d vector) =
  vector


zero: Vector3d
zero =
  Vector3d 0 0 0


fromTuple: (Float, Float, Float) -> Vector3d
fromTuple (x, y, z) =
  Vector3d x y z


toTuple: Vector3d -> (Float, Float, Float)
toTuple (Vector3d x y z) =
  (x, y, z)


equals: Vector3d -> Vector3d -> Bool
equals (Vector3d x2 y2 z2) (Vector3d x1 y1 z1) =
  x1 == x2 && y1 == y2 && z1 == z2


componentIn: Direction3d -> Vector3d -> Float
componentIn (Direction3d vector) =
  dot vector


squaredLength: Vector3d -> Float
squaredLength (Vector3d x y z) =
  x * x + y * y + z * z


length: Vector3d -> Float
length =
  squaredLength >> sqrt


normalized: Vector3d -> Maybe Vector3d
normalized vector =
  if equals zero vector then Nothing else Just (times (1 / length vector) vector)


direction: Vector3d -> Maybe Direction3d
direction =
  normalized >> Maybe.map Direction3d


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


normalDirection: Vector3d -> Maybe Direction3d
normalDirection =
  perpendicularVector >> direction


projectionIn: Direction3d -> Vector3d -> Vector3d
projectionIn direction vector =
    times (componentIn direction vector) (toVector direction)


projectedOnto: Plane3d -> Vector3d -> Vector3d
projectedOnto plane vector =
  minus (projectionIn plane.normalDirection vector) vector


projectedInto: Plane3d -> Vector3d -> Vector2d
projectedInto plane vector =
  Matrix3x2.dotProduct plane.xDirection plane.yDirection vector


negated: Vector3d -> Vector3d
negated (Vector3d x y z) =
  Vector3d (-x) (-y) (-z)


plus: Vector3d -> Vector3d -> Vector3d
plus (Vector3d x2 y2 z2) (Vector3d x1 y1 z1) =
  Vector3d (x1 + x2) (y1 + y2) (z1 + z2)


minus: Vector3d -> Vector3d -> Vector3d
minus (Vector3d x2 y2 z2) (Vector3d x1 y1 z1) =
  Vector3d (x1 - x2) (y1 - y2) (z1 - z2)


times: Float -> Vector3d -> Vector3d
times scale (Vector3d x y z) =
  Vector3d (x * scale) (y * scale) (z * scale)


addedTo: Point3d -> Vector3d -> Point3d
addedTo (Point3d px py pz) (Vector3d vx vy vz) =
  Point3d (px + vx) (py + vy) (pz + vz)


subtractedFrom: Point3d -> Vector3d -> Point3d
subtractedFrom (Point3d px py pz) (Vector3d vx vy vz) =
  Point3d (px - vx) (py - vy) (pz - vz)


dot: Vector3d -> Vector3d -> Float
dot (Vector3d x2 y2 z2) (Vector3d x1 y1 z1) =
  x1 * x2 + y1 * y2 + z1 * z2


cross: Vector3d -> Vector3d -> Vector3d
cross (Vector3d x2 y2 z2) (Vector3d x1 y1 z1) =
  Vector3d (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)
