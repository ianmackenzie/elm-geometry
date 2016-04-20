module OpenSolid.Core.Matrix2x2
  ( product
  , dotProduct
  ) where


import OpenSolid.Core exposing (..)


product: Direction2d -> Direction2d -> Vector2d -> Vector2d
product (Direction2d x1 y1) (Direction2d x2 y2) (Vector2d x y) =
  Vector2d (x1 * x + x2 * y) (y1 * x + y2 * y)


dotProduct: Direction2d -> Direction2d -> Vector2d -> Vector2d
dotProduct (Direction2d x1 y1) (Direction2d x2 y2) (Vector2d x y) =
  Vector2d (x1 * x + y1 * y) (x2 * x + y2 * y)
