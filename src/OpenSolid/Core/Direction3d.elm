module OpenSolid.Core.Direction3d
  ( none
  , xDirection
  , yDirection
  , zDirection
  , vector
  , xComponent
  , yComponent
  , zComponent
  , components
  , normalDirection
  , transformedBy
  , negated
  , times
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Vector3d as Vector3d


none: Direction3d
none =
  Direction3d Vector3d.zero


xDirection: Direction3d
xDirection =
  Direction3d (Vector3d 1 0 0)


yDirection: Direction3d
yDirection =
  Direction3d (Vector3d 0 1 0)


zDirection: Direction3d
zDirection =
  Direction3d (Vector3d 0 0 1)


vector: Direction3d -> Vector3d
vector (Direction3d representation) =
  representation


xComponent: Direction3d -> Float
xComponent (Direction3d vector) =
  Vector3d.xComponent vector


yComponent: Direction3d -> Float
yComponent (Direction3d vector) =
  Vector3d.yComponent vector


zComponent: Direction3d -> Float
zComponent (Direction3d vector) =
  Vector3d.zComponent vector


components: Direction3d -> (Float, Float, Float)
components (Direction3d vector) =
  Vector3d.components vector


normalDirection: Direction3d -> Direction3d
normalDirection (Direction3d vector) =
  Vector3d.normalDirection vector


transformedBy: Transformation3d -> Direction3d -> Direction3d
transformedBy (Transformation3d transformVector transformPoint) (Direction3d vector) =
  Direction3d (transformVector vector)


negated: Direction3d -> Direction3d
negated (Direction3d vector) =
  Direction3d (Vector3d.negated vector)


times: Float -> Direction3d -> Vector3d
times scale (Direction3d vector) =
  Vector3d.times scale vector
