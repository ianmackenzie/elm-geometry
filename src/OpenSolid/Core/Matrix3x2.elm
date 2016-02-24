module OpenSolid.Core.Matrix3x2
  ( product
  , dotProduct
  ) where


import OpenSolid.Core exposing (..)


product: Direction3d -> Direction3d -> Vector2d -> Vector3d
product xDirection yDirection vector =
  let
    x = xDirection.x * vector.x + yDirection.x * vector.y
    y = xDirection.y * vector.x + yDirection.y * vector.y
    z = xDirection.z * vector.x + yDirection.z * vector.y
  in
    Vector3d x y z


dotProduct: Direction3d -> Direction3d -> Vector3d -> Vector2d
dotProduct xDirection yDirection vector =
  let
    x = xDirection.x * vector.x + xDirection.y * vector.y + xDirection.z * vector.z
    y = yDirection.x * vector.x + yDirection.y * vector.y + yDirection.z * vector.z
  in
    Vector2d x y
