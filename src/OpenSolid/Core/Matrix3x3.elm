module OpenSolid.Core.Matrix3x3
  ( product
  , dotProduct
  ) where


import OpenSolid.Core exposing (..)


product: Direction3d -> Direction3d -> Direction3d -> Vector3d -> Vector3d
product direction1 direction2 direction3 vector =
  let
    (Direction3d (Vector3d x1 y1 z1)) = direction1
    (Direction3d (Vector3d x2 y2 z2)) = direction2
    (Direction3d (Vector3d x3 y3 z3)) = direction3
    (Vector3d x y z) = vector
  in
    Vector3d (x1 * x + x2 * y + x3 * z) (y1 * x + y2 * y + y3 * z) (z1 * x + z2 * y + z3 * z)


dotProduct: Direction3d -> Direction3d -> Direction3d -> Vector3d -> Vector3d
dotProduct direction1 direction2 direction3 vector =
  let
    (Direction3d (Vector3d x1 y1 z1)) = direction1
    (Direction3d (Vector3d x2 y2 z2)) = direction2
    (Direction3d (Vector3d x3 y3 z3)) = direction3
    (Vector3d x y z) = vector
  in
    Vector3d (x1 * x + y1 * y + z1 * z) (x2 * x + y2 * y + z2 * z) (x3 * x + y3 * y + z3 * z)
