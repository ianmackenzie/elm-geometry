module OpenSolid.Core.Point2d
  ( origin
  , polar
  , components
  , squaredDistanceTo
  , distanceTo
  , distanceAlongAxis
  , distanceToAxis
  , scaledAbout
  , transformedBy
  , projectedOntoAxis
  , placedOntoPlane
  , plus
  , minus
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Direction2d as Direction2d
import OpenSolid.Core.Math2d as Math2d
import OpenSolid.Core.Math3d as Math3d


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
  squaredDistanceTo other >> sqrt


distanceAlongAxis: Axis2d -> Point2d -> Float
distanceAlongAxis axis =
  minus axis.originPoint >> Vector2d.dot axis.direction


distanceToAxis: Axis2d -> Point2d -> Float
distanceToAxis axis =
  minus axis.originPoint >> Vector2d.dot (Direction2d.normalDirection axis.direction)


scaledAbout: Point2d -> Float -> Point2d -> Point2d
scaledAbout originPoint scale point =
  let
    displacement = minus originPoint point
  in
    plus (Vector2d.times scale displacement) originPoint


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
  Math3d.plus (Vector2d.placedOntoPlane plane point) plane.originPoint


plus: Vector2d -> Point2d -> Point2d
plus =
  Math2d.plus


minus: Point2d -> Point2d -> Vector2d
minus =
  Math2d.minus
