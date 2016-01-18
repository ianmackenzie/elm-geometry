module OpenSolid.Core.Point3d
  ( origin
  , components
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
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Math3d as Math3d


origin: Point3d
origin =
  Point3d 0 0 0


components: Point3d -> (Float, Float, Float)
components point =
  (point.x, point.y, point.z)


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
  minus plane.originPoint >> Vector3d.dot plane.normalDirection


scaledAbout: Point3d -> Float -> Point3d -> Point3d
scaledAbout originPoint scale point =
  let
    displacement = minus originPoint point
  in
    plus (Vector3d.times scale displacement) originPoint


transformedBy: Transformation3d -> Point3d -> Point3d
transformedBy =
  snd


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
    normalDisplacement = Vector3d.times distance plane.normalDirection
  in
    Math3d.minus normalDisplacement point


projectedIntoPlane: Plane3d -> Point3d -> Point2d
projectedIntoPlane plane =
  minus plane.originPoint >> Vector3d.projectedIntoPlane plane


plus: Vector3d -> Point3d -> Point3d
plus =
  Math3d.plus


minus: Point3d -> Point3d -> Vector3d
minus =
  Math3d.minus


hull: Point3d -> Point3d -> Box3d
hull firstPoint secondPoint =
  let
    x = Scalar.hull firstPoint.x secondPoint.x
    y = Scalar.hull firstPoint.y secondPoint.y
    z = Scalar.hull firstPoint.z secondPoint.z
  in
    Box3d x y z
