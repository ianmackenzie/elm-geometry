module OpenSolid.Point2d
  ( Point2d(Point2d)
  , origin
  , xComponent
  , yComponent
  , components
  , squaredDistance
  , distance
  , sum
  , difference
  ) where


import OpenSolid.Vector2d as Vector2d exposing (Vector2d(Vector2d))


type Point2d =
  Point2d Float Float


origin: Point2d
origin =
  Point2d 0.0 0.0


xComponent: Point2d -> Float
xComponent (Point2d x y) =
  x


yComponent: Point2d -> Float
yComponent (Point2d x y) =
  y


components: Point2d -> (Float, Float)
components (Point2d x y) =
  (x, y)


squaredDistance: Point2d -> Point2d -> Float
squaredDistance firstPoint secondPoint =
  Vector2d.squaredLength (difference firstPoint secondPoint)



distance: Point2d -> Point2d -> Float
distance firstPoint secondPoint =
  Vector2d.length (difference firstPoint secondPoint)


sum: Point2d -> Vector2d -> Point2d
sum (Point2d x1 y1) (Vector2d x2 y2) =
  Point2d (x1 + x2) (y1 + y2)


difference: Point2d -> Point2d -> Vector2d
difference (Point2d x1 y1) (Point2d x2 y2) =
  Vector2d (x1 - x2) (y1 - y2)
