module OpenSolid.Core.Point2d
  ( origin
  , polar
  , toTuple
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
  , hull
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Scalar as Scalar
import OpenSolid.Core.Components2d as Components2d
import OpenSolid.Core.Components3d as Components3d
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Direction2d as Direction2d


origin: Point2d
origin =
  Point2d 0 0


polar: Float -> Float -> Point2d
polar radius angle =
  Point2d (radius * cos angle) (radius * sin angle)


toTuple: Point2d -> (Float, Float)
toTuple =
  Components2d.toTuple


squaredDistanceTo: Point2d -> Point2d -> Float
squaredDistanceTo other =
  minus other >> Vector2d.squaredLength


distanceTo: Point2d -> Point2d -> Float
distanceTo other =
  squaredDistanceTo other >> sqrt


distanceAlongAxis: Axis2d -> Point2d -> Float
distanceAlongAxis axis =
  minus axis.originPoint >> Vector2d.componentIn axis.direction


distanceToAxis: Axis2d -> Point2d -> Float
distanceToAxis axis =
  minus axis.originPoint >> Vector2d.componentIn (Direction2d.normalDirection axis.direction)


scaledAbout: Point2d -> Float -> Point2d -> Point2d
scaledAbout originPoint scale point =
  let
    displacement = minus originPoint point
  in
    plus (Vector2d.times scale displacement) originPoint


transformedBy: Transformation2d -> Point2d -> Point2d
transformedBy transformation =
  transformation.transformPoint


projectedOntoAxis: Axis2d -> Point2d -> Point2d
projectedOntoAxis axis point =
  let
    displacement = minus axis.originPoint point
    projectedDisplacement = Vector2d.projectedOntoAxis axis displacement
  in
    plus projectedDisplacement axis.originPoint


placedOntoPlane: Plane3d -> Point2d -> Point3d
placedOntoPlane plane =
  Vector2d.placedOntoPlane plane >> Components3d.plus plane.originPoint


plus: Vector2d -> Point2d -> Point2d
plus =
  Components2d.plus


minus: Point2d -> Point2d -> Vector2d
minus =
  Components2d.minus


hull: Point2d -> Point2d -> Bounds2d
hull other point =
  Bounds2d (Scalar.hull other.x point.x) (Scalar.hull other.y point.y)
