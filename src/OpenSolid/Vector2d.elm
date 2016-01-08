module OpenSolid.Vector2d
  ( Vector2d(Vector2d)
  , zero
  , unitVector
  , xComponent
  , yComponent
  , components
  , squaredLength
  , length
  , normalized
  , direction
  , normalDirection
  , negated
  , sum
  , difference
  , product
  , dotProduct
  , crossProduct
  ) where


import OpenSolid.Direction2d as Direction2d exposing (Direction2d(Direction2d))


type Vector2d =
  Vector2d Float Float


zero: Vector2d
zero =
  Vector2d 0.0 0.0


unitVector: Direction2d -> Vector2d
unitVector (Direction2d x y) =
  Vector2d x y


xComponent: Vector2d -> Float
xComponent (Vector2d x y) =
  x


yComponent: Vector2d -> Float
yComponent (Vector2d x y) =
  y


components: Vector2d -> (Float, Float)
components (Vector2d x y) =
  (x, y)


squaredLength: Vector2d -> Float
squaredLength (Vector2d x y) =
  x^2 + y^2


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
      product (1 / length') vector


direction: Vector2d -> Direction2d
direction vector =
  let
    (Vector2d x y) = normalized vector
  in
    Direction2d x y

normalDirection: Vector2d -> Direction2d
normalDirection vector =
  Direction2d.normalDirection (direction vector)


negated: Vector2d -> Vector2d
negated (Vector2d x y) =
  Vector2d (-x) (-y)


sum: Vector2d -> Vector2d -> Vector2d
sum (Vector2d x1 y1) (Vector2d x2 y2) =
  Vector2d (x1 + x2) (y1 + y2)


difference: Vector2d -> Vector2d -> Vector2d
difference (Vector2d x1 y1) (Vector2d x2 y2) =
  Vector2d (x1 - x2) (y1 - y2)


product: Float -> Vector2d -> Vector2d
product scale (Vector2d x y) =
  Vector2d (scale * x) (scale * y)


dotProduct: Vector2d -> Vector2d -> Float
dotProduct (Vector2d x1 y1) (Vector2d x2 y2) =
  x1 * x2 + y1 * y2


crossProduct: Vector2d -> Vector2d -> Float
crossProduct (Vector2d x1 y1) (Vector2d x2 y2) =
  x1 * y2 - y1 * x2
