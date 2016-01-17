module OpenSolid.Core.Point2d
  ( origin
  , polar
  , components
  , squaredDistanceTo
  , distanceTo
  , distanceAlongAxis
  , distanceToAxis
  , transformedBy
  , projectedOntoAxis
  , placedOntoPlane
  , plus
  , minus
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Point3d as Point3d


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


distanceAlongAxis: Axis2d -> Point2d -> Float
distanceAlongAxis axis =
  minus axis.originPoint >> Vector2d.dot axis.direction


distanceToAxis: Axis2d -> Point2d -> Float
distanceToAxis axis =
  minus axis.originPoint >> Vector2d.dot (Axis2d.normalDirection axis)


transformedBy: Transformation2d -> Point2d -> Point2d
transformedBy =
  snd


projectedOntoAxis: Axis2d -> Point2d -> Point2d
projectedOntoAxis axis point =
  let
    displacement = minus axis.originPoint point
    projectedDisplacement = Vector2d.projectedOntoAxis axis displacement
  in
    plus projectedDisplacement axis.originPoint


placedOntoPlane: Plane3d -> Point2d -> Point3d
placedOntoPlane plane point =
  Point3d.plus (Vector2d.placedOntoPlane plane point) plane.originPoint


plus: Vector2d -> Point2d -> Point2d
plus vector point =
  Point2d (point.x + vector.x) (point.y + vector.y)


minus: Point2d -> Point2d -> Vector2d
minus other point =
  Vector2d (point.x - other.x) (point.y - other.y)
