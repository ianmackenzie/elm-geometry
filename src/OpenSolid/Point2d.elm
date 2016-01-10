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
  Point2d (radius * (cos angle)) (radius * (sin angle))


components: Point2d -> (Float, Float)
components point =
  (point.x, point.y)


squaredDistanceTo: Point2d -> Point2d -> Float
squaredDistanceTo firstPoint secondPoint =
  Vector2d.squaredLength (secondPoint `minus` firstPoint)



distanceTo: Point2d -> Point2d -> Float
distanceTo firstPoint secondPoint =
  Vector2d.length (secondPoint `minus` firstPoint)


transformedBy: Transformation2d -> Point2d -> Point2d
transformedBy transformation point =
  transformation.ofPoint point


plus: Point2d -> Vector2d -> Point2d
plus point vector  =
  Point2d (point.x + vector.x) (point.y + vector.y)


minus: Point2d -> Point2d -> Vector2d
minus firstPoint secondPoint =
  Vector2d (firstPoint.x - secondPoint.x) (firstPoint.y - secondPoint.y)
