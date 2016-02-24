module OpenSolid.Core.Matrix2x2
  ( product
  , dotProduct
  ) where


import OpenSolid.Core exposing (..)


product: Direction2d -> Direction2d -> Vector2d -> Vector2d
product xDirection yDirection vector =
  let
    x = xDirection.x * vector.x + yDirection.x * vector.y
    y = xDirection.y * vector.x + yDirection.y * vector.y
  in
    Vector2d x y


dotProduct: Direction2d -> Direction2d -> Vector2d -> Vector2d
dotProduct xDirection yDirection vector =
  let
    x = xDirection.x * vector.x + xDirection.y * vector.y
    y = yDirection.x * vector.x + yDirection.y * vector.y
  in
    Vector2d x y
