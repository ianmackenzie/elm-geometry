module OpenSolid.Core.Direction3d
  ( x
  , y
  , z
  , fromTuple
  , toTuple
  , vector
  , normalDirection
  , normalBasis
  , transformedBy
  , projectedOntoPlane
  , projectedIntoPlane
  , negated
  , times
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Vector3d as Vector3d


toDirection3d: Vector3d -> Direction3d
toDirection3d (Vector3d x y z) =
  Direction3d x y z


x: Direction3d
x =
  Direction3d 1 0 0


y: Direction3d
y =
  Direction3d 0 1 0


z: Direction3d
z =
  Direction3d 0 0 1


fromTuple: (Float, Float, Float) -> Direction3d
fromTuple (x, y, z) =
  Direction3d x y z


toTuple: Direction3d -> (Float, Float, Float)
toTuple (Direction3d x y z) =
  (x, y, z)


vector: Direction3d -> Vector3d
vector (Direction3d x y z) =
  Vector3d x y z


normalDirection: Direction3d -> Direction3d
normalDirection direction =
  let
    (Vector3d x y z as perpendicularVector) = Vector3d.perpendicularVector (vector direction)
    scale = 1 / (Vector3d.length perpendicularVector)
  in
    Direction3d (x * scale) (y * scale) (z * scale)


normalBasis: Direction3d -> (Direction3d, Direction3d)
normalBasis direction =
  let
    xDirection = normalDirection direction
    yDirection = toDirection3d (Vector3d.cross (vector xDirection) (vector direction))
  in
    (xDirection, yDirection)


transformedBy: Transformation3d -> Direction3d -> Direction3d
transformedBy transformation =
  vector >> Vector3d.transformedBy transformation >> toDirection3d


projectedOntoPlane: Plane3d -> Direction3d -> Maybe Direction3d
projectedOntoPlane plane =
  vector >> Vector3d.projectedOntoPlane plane >> Vector3d.direction


projectedIntoPlane: Plane3d -> Direction3d -> Maybe Direction2d
projectedIntoPlane plane =
  vector >> Vector3d.projectedIntoPlane plane >> Vector2d.direction


negated: Direction3d -> Direction3d
negated (Direction3d x y z) =
  Direction3d (-x) (-y) (-z)


times: Float -> Direction3d -> Vector3d
times scale (Direction3d x y z) =
  Vector3d (x * scale) (y * scale) (z * scale)
