module OpenSolid.Core.Direction3d
  ( none
  , x
  , y
  , z
  , components
  , normalDirection
  , transformedBy
  , projectedOntoPlane
  , projectedIntoPlane
  , negated
  , times
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Vector3d as Vector3d


none: Direction3d
none =
  Direction3d Vector3d.zero


x: Direction3d
x =
  Direction3d (Vector3d 1 0 0)


y: Direction3d
y =
  Direction3d (Vector3d 0 1 0)


z: Direction3d
z =
  Direction3d (Vector3d 0 0 1)


components: Direction3d -> (Float, Float, Float)
components (Direction3d vector) =
  Vector3d.components vector


normalDirection: Direction3d -> Direction3d
normalDirection (Direction3d vector) =
  Vector3d.normalDirection vector


transformedBy: Transformation3d -> Direction3d -> Direction3d
transformedBy transformation (Direction3d vector) =
  Direction3d (Vector3d.transformedBy transformation vector)


projectedOntoPlane: Plane3d -> Direction3d -> Direction3d
projectedOntoPlane plane (Direction3d vector) =
  Vector3d.direction (Vector3d.projectedOntoPlane plane vector)


projectedIntoPlane: Plane3d -> Direction3d -> Direction2d
projectedIntoPlane plane (Direction3d vector) =
  Vector2d.direction (Vector3d.projectedIntoPlane plane vector)


negated: Direction3d -> Direction3d
negated (Direction3d vector) =
  Direction3d (Vector3d.negated vector)


times: Float -> Direction3d -> Vector3d
times scale (Direction3d vector) =
  Vector3d.times scale vector
