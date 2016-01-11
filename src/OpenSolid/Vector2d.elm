module OpenSolid.Vector2d
  ( Vector2d
  , zero
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


import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.Transformation2d as Transformation2d exposing (Transformation2d)

type alias Vector2d =
  { x : Float
  , y : Float
  }

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
transformedBy transformation =
  transformation.ofVector


negated: Vector2d -> Vector2d
negated vector =
  Vector2d (-vector.x) (-vector.y)


plus: Vector2d -> Vector2d -> Vector2d
plus otherVector vector =
  Vector2d (vector.x + otherVector.x) (vector.y + otherVector.y)


minus: Vector2d -> Vector2d -> Vector2d
minus otherVector vector =
  Vector2d (vector.x - otherVector.x) (vector.y - otherVector.y)


times: Float -> Vector2d -> Vector2d
times scale vector =
  Vector2d (vector.x * scale) (vector.y * scale)


dot: Vector2d -> Vector2d -> Float
dot otherVector vector =
  vector.x * otherVector.x + vector.y * otherVector.y


cross: Vector2d -> Vector2d -> Float
cross otherVector vector =
  vector.x * otherVector.y - vector.y * otherVector.x
