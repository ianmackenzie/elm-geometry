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
length vector =
  sqrt (squaredLength vector)


normalized: Vector2d -> Vector2d
normalized vector =
  let
    length' = length vector
  in
    if length' == 0 then
      zero
    else
      times vector (1 / length')


direction: Vector2d -> Direction2d
direction vector =
  normalized vector


normalDirection: Vector2d -> Direction2d
normalDirection vector =
  Direction2d.normalDirection (direction vector)


transformedBy: Transformation2d -> Vector2d -> Vector2d
transformedBy transformation vector =
  transformation.ofVector vector


negated: Vector2d -> Vector2d
negated vector =
  Vector2d (-vector.x) (-vector.y)


plus: Vector2d -> Vector2d -> Vector2d
plus firstVector secondVector =
  Vector2d (firstVector.x + secondVector.x) (firstVector.y + secondVector.y)


minus: Vector2d -> Vector2d -> Vector2d
minus firstVector secondVector =
  Vector2d (firstVector.x - secondVector.x) (firstVector.y - secondVector.y)


times: Vector2d -> Float -> Vector2d
times vector scale =
  Vector2d (vector.x * scale) (vector.y * scale)


dot: Vector2d -> Vector2d -> Float
dot firstVector secondVector =
  firstVector.x * secondVector.x + firstVector.y * secondVector.y


cross: Vector2d -> Vector2d -> Float
cross firstVector secondVector =
  firstVector.x * secondVector.y - firstVector.y * secondVector.x
