module OpenSolid.Core.Point2d
  ( origin
  , polar
  , components
  , squaredDistanceTo
  , distanceTo
  , transformedBy
  , plus
  , minus
  ) where


import OpenSolid.Core exposing (Point2d, Vector2d, Transformation2d)
import OpenSolid.Core.Vector2d as Vector2d


origin: Point2d
origin =
  Point2d 0 0


polar: Float -> Float -> Point2d
polar radius angle =
  Point2d (radius * cos angle) (radius * sin angle)


components: Point2d -> (Float, Float)
components point =
  (point.x, point.y)


squaredDistanceTo: Point2d -> Point2d -> Float
squaredDistanceTo other =
  minus other >> Vector2d.squaredLength


distanceTo: Point2d -> Point2d -> Float
distanceTo other =
  minus other >> Vector2d.length


transformedBy: Transformation2d -> Point2d -> Point2d
transformedBy =
  .ofPoint


plus: Vector2d -> Point2d -> Point2d
plus vector point =
  Point2d (point.x + vector.x) (point.y + vector.y)


minus: Point2d -> Point2d -> Vector2d
minus other point =
  Vector2d (point.x - other.x) (point.y - other.y)
