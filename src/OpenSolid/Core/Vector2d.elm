module OpenSolid.Core.Vector2d
  ( zero
  , components
  , squaredLength
  , length
  , normalized
  , direction
  , normalDirection
  , transformedBy
  , negated
  , plus
  , minus
  , times
  , dot
  , cross
  ) where


import OpenSolid.Core exposing (Vector2d, Direction2d, Transformation2d)
import OpenSolid.Core.Direction2d as Direction2d


zero: Vector2d
zero =
  Vector2d 0 0


components: Vector2d -> (Float, Float)
components vector =
  (vector.x, vector.y)


squaredLength: Vector2d -> Float
squaredLength vector =
  vector.x^2 + vector.y^2


length: Vector2d -> Float
length =
  squaredLength >> sqrt


normalized: Vector2d -> Vector2d
normalized vector =
  let
    length' = length vector
  in
    if length' == 0 then
      zero
    else
      times (1 / length') vector


direction: Vector2d -> Direction2d
direction =
  normalized


normalDirection: Vector2d -> Direction2d
normalDirection =
  direction >> Direction2d.normalDirection


transformedBy: Transformation2d -> Vector2d -> Vector2d
transformedBy =
  fst


negated: Vector2d -> Vector2d
negated vector =
  Vector2d (-vector.x) (-vector.y)


plus: Vector2d -> Vector2d -> Vector2d
plus other vector =
  Vector2d (vector.x + other.x) (vector.y + other.y)


minus: Vector2d -> Vector2d -> Vector2d
minus other vector =
  Vector2d (vector.x - other.x) (vector.y - other.y)


times: Float -> Vector2d -> Vector2d
times scale vector =
  Vector2d (vector.x * scale) (vector.y * scale)


dot: Vector2d -> Vector2d -> Float
dot other vector =
  vector.x * other.x + vector.y * other.y


cross: Vector2d -> Vector2d -> Float
cross other vector =
  vector.x * other.y - vector.y * other.x
