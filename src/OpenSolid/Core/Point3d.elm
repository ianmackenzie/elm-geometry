module OpenSolid.Core.Point3d
  ( origin
  , toTuple
  , squaredDistanceTo
  , distanceTo
  , squaredDistanceToAxis
  , distanceToAxis
  , distanceToPlane
  , scaledAbout
  , transformedBy
  , projectedOntoAxis
  , projectedOntoPlane
  , projectedIntoPlane
  , plus
  , minus
  , hull
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Scalar as Scalar
import OpenSolid.Core.Components3d as Components3d
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Direction3d as Direction3d


origin: Point3d
origin =
  Point3d 0 0 0


toTuple: Point3d -> (Float, Float, Float)
toTuple =
  Components3d.toTuple


squaredDistanceTo: Point3d -> Point3d -> Float
squaredDistanceTo other =
  minus other >> Vector3d.squaredLength


distanceTo: Point3d -> Point3d -> Float
distanceTo other =
  squaredDistanceTo other >> sqrt


squaredDistanceToAxis: Axis3d -> Point3d -> Float
squaredDistanceToAxis axis =
  minus axis.originPoint >> Vector3d.cross axis.direction >> Vector3d.squaredLength


distanceToAxis: Axis3d -> Point3d -> Float
distanceToAxis axis =
  squaredDistanceToAxis axis >> sqrt


distanceToPlane: Plane3d -> Point3d -> Float
distanceToPlane plane =
  minus plane.originPoint >> Vector3d.componentIn plane.normalDirection


scaledAbout: Point3d -> Float -> Point3d -> Point3d
scaledAbout originPoint scale point =
  let
    displacement = minus originPoint point
  in
    plus (Vector3d.times scale displacement) originPoint


transformedBy: Transformation3d -> Point3d -> Point3d
transformedBy transformation =
  transformation.transformPoint


projectedOntoAxis: Axis3d -> Point3d -> Point3d
projectedOntoAxis axis point =
  let
    displacement = minus axis.originPoint point
    projectedDisplacement = Vector3d.projectedOntoAxis axis displacement
  in
    plus projectedDisplacement axis.originPoint


projectedOntoPlane: Plane3d -> Point3d -> Point3d
projectedOntoPlane plane point =
  let
    distance = distanceToPlane plane point
    normalDisplacement = Direction3d.times (-distance) plane.normalDirection
  in
    plus normalDisplacement point


projectedIntoPlane: Plane3d -> Point3d -> Point2d
projectedIntoPlane plane point =
  Vector3d.projectedIntoPlane plane (minus plane.originPoint point)


plus: Vector3d -> Point3d -> Point3d
plus =
  Components3d.plus


minus: Point3d -> Point3d -> Vector3d
minus =
  Components3d.minus


hull: Point3d -> Point3d -> Bounds3d
hull other point =
  Bounds3d (Scalar.hull other.x point.x) (Scalar.hull other.y point.y) (Scalar.hull other.z point.z)
