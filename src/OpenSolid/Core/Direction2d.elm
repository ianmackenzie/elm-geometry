module OpenSolid.Core.Direction2d
  ( x
  , y
  , polar
  , fromTuple
  , toTuple
  , normalDirection
  , placedOntoPlane
  , negated
  , times
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Vector2d as Vector2d


toDirection2d: Vector2d -> Direction2d
toDirection2d (Vector2d x y) =
  Direction2d x y


toDirection3d: Vector3d -> Direction3d
toDirection3d (Vector3d x y z) =
  Direction3d x y z


x: Direction2d
x =
  Direction2d 1 0


y: Direction2d
y =
  Direction2d 0 1


polar: Float -> Direction2d
polar angle =
  Direction2d (cos angle) (sin angle)


fromTuple: (Float, Float) -> Direction2d
fromTuple (x, y) =
  Direction2d x y


toTuple: Direction2d -> (Float, Float)
toTuple (Direction2d x y) =
  (x, y)


vector: Direction2d -> Vector2d
vector (Direction2d x y) =
  Vector2d x y


normalDirection: Direction2d -> Direction2d
normalDirection (Direction2d x y) =
  Direction2d (-y) x


placedOntoPlane: Plane3d -> Direction2d -> Direction3d
placedOntoPlane plane =
  vector >> Vector2d.placedOntoPlane plane >> toDirection3d


negated: Direction2d -> Direction2d
negated (Direction2d x y) =
  Direction2d (-x) (-y)


times: Float -> Direction2d -> Vector2d
times scale (Direction2d x y) =
  Vector2d (x * scale) (y * scale)
