module OpenSolid.Point2d
  ( Point2d
  , origin
  , polar
  , components
  , squaredDistanceTo
  , distanceTo
  , transformedBy
  , plus
  , minus
  ) where


import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import OpenSolid.Transformation2d as Transformation2d exposing (Transformation2d)


type alias Point2d =
  { x : Float
  , y : Float
  }


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
squaredDistanceTo otherPoint =
  minus otherPoint >> Vector2d.squaredLength



distanceTo: Point2d -> Point2d -> Float
distanceTo otherPoint =
  minus otherPoint >> Vector2d.length


transformedBy: Transformation2d -> Point2d -> Point2d
transformedBy transformation =
  transformation.ofPoint


plus: Vector2d -> Point2d -> Point2d
plus vector point =
  Point2d (point.x + vector.x) (point.y + vector.y)


minus: Point2d -> Point2d -> Vector2d
minus otherPoint point =
  Vector2d (point.x - otherPoint.x) (point.y - otherPoint.y)
