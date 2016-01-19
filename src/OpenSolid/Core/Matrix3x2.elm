module OpenSolid.Core.Matrix3x2
  ( product
  , dotProduct
  ) where


import OpenSolid.Core exposing (..)


product: Direction3d -> Direction3d -> Vector2d -> Vector3d
product (Direction3d (Vector3d x1 y1 z1)) (Direction3d (Vector3d x2 y2 z2)) (Vector2d x y) =
  Vector3d (x1 * x + x2 * y) (y1 * x + y2 * y) (z1 * x + z2 * y)


dotProduct: Direction3d -> Direction3d -> Vector3d -> Vector2d
dotProduct (Direction3d (Vector3d x1 y1 z1)) (Direction3d (Vector3d x2 y2 z2)) (Vector3d x y z) =
  Vector2d (x1 * x + y1 * y + z1 * z) (x2 * x + y2 * y + z2 * z)
