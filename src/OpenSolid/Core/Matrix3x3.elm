module OpenSolid.Core.Matrix3x3
  ( product
  , dotProduct
  ) where


import OpenSolid.Core exposing (..)


product: Direction3d -> Direction3d -> Direction3d -> Vector3d -> Vector3d
product xDirection yDirection zDirection vector =
  let
    x = xDirection.x * vector.x + yDirection.x * vector.y + zDirection.x * vector.z
    y = xDirection.y * vector.x + yDirection.y * vector.y + zDirection.y * vector.z
    z = xDirection.z * vector.x + yDirection.z * vector.y + zDirection.z * vector.z
  in
    Vector3d x y z


dotProduct: Direction3d -> Direction3d -> Direction3d -> Vector3d -> Vector3d
dotProduct xDirection yDirection zDirection vector =
  let
    x = xDirection.x * vector.x + xDirection.y * vector.y + xDirection.z * vector.z
    y = yDirection.x * vector.x + yDirection.y * vector.y + yDirection.z * vector.z
    z = zDirection.x * vector.x + zDirection.y * vector.y + zDirection.z * vector.z
  in
    Vector3d x y z
