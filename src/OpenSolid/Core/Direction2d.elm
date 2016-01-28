module OpenSolid.Core.Direction2d
  ( none
  , x
  , y
  , polar
  , components
  , normalDirection
  , transformedBy
  , placedOntoPlane
  , negated
  , times
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Vector2d as Vector2d


none: Direction2d
none =
  Direction2d Vector2d.zero


x: Direction2d
x =
  Direction2d (Vector2d 1 0)


y: Direction2d
y =
  Direction2d (Vector2d 0 1)


polar: Float -> Direction2d
polar angle =
  Direction2d (Vector2d (cos angle) (sin angle))


components: Direction2d -> (Float, Float)
components (Direction2d vector) =
  Vector2d.components vector


normalDirection: Direction2d -> Direction2d
normalDirection (Direction2d (Vector2d x y)) =
  Direction2d (Vector2d (-y) x)


transformedBy: Transformation2d -> Direction2d -> Direction2d
transformedBy transformation (Direction2d vector) =
  Direction2d (Vector2d.transformedBy transformation vector)


placedOntoPlane: Plane3d -> Direction2d -> Direction3d
placedOntoPlane plane (Direction2d vector) =
  Direction3d (Vector2d.placedOntoPlane plane vector)


negated: Direction2d -> Direction2d
negated (Direction2d vector) =
  Direction2d (Vector2d.negated vector)


times: Float -> Direction2d -> Vector2d
times scale (Direction2d vector) =
  Vector2d.times scale vector
