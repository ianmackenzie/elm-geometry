module OpenSolid.Core.Point3d
  ( origin
  , xComponent
  , yComponent
  , zComponent
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


origin: Point3d
origin =
  Point3d 0 0 0


xComponent: Point3d -> Float
xComponent (Point3d x y z) =
  x


yComponent: Point3d -> Float
yComponent (Point3d x y z) =
  y


zComponent: Point3d -> Float
zComponent (Point3d x y z) =
  z


components: Point3d -> (Float, Float, Float)
components (Point3d x y z) =
  (x, y, z)


squaredDistanceTo: Point3d -> Point3d -> Float
squaredDistanceTo other =
  minus other >> Vector3d.squaredLength


distanceTo: Point3d -> Point3d -> Float
distanceTo other =
  squaredDistanceTo other >> sqrt


squaredDistanceToAxis: Axis3d -> Point3d -> Float
squaredDistanceToAxis (Axis3d originPoint (Direction3d directionVector)) =
  minus originPoint >> Vector3d.cross directionVector >> Vector3d.squaredLength


distanceToAxis: Axis3d -> Point3d -> Float
distanceToAxis axis =
  squaredDistanceToAxis axis >> sqrt


distanceToPlane: Plane3d -> Point3d -> Float
distanceToPlane (Plane3d originPoint xDirection yDirection (Direction3d normalVector)) =
  minus originPoint >> Vector3d.dot normalVector


scaledAbout: Point3d -> Float -> Point3d -> Point3d
scaledAbout originPoint scale point =
  let
    displacement = minus originPoint point
  in
    plus (Vector3d.times scale displacement) originPoint


transformedBy: Transformation3d -> Point3d -> Point3d
transformedBy (Transformation3d transformVector transformPoint) =
  transformPoint


projectedOntoAxis: Axis3d -> Point3d -> Point3d
projectedOntoAxis axis point =
  let
    (Axis3d originPoint direction) = axis
    projectedDisplacement = Vector3d.projectedOntoAxis axis (minus originPoint point)
  in
    plus projectedDisplacement originPoint


projectedOntoPlane: Plane3d -> Point3d -> Point3d
projectedOntoPlane plane point =
  let
    (Plane3d originPoint xDirection yDirection (Direction3d normalVector)) = plane
    distance = distanceToPlane plane point
    normalDisplacement = Vector3d.times (-distance) normalVector
  in
    plus normalDisplacement point


projectedIntoPlane: Plane3d -> Point3d -> Point2d
projectedIntoPlane plane point =
  let
    (Plane3d originPoint xDirection yDirection normalDirection) = plane
    displacement = minus originPoint point
    (Vector2d x y) = Vector3d.projectedIntoPlane plane displacement
  in
    Point2d x y


plus: Vector3d -> Point3d -> Point3d
plus (Vector3d vectorX vectorY vectorZ) (Point3d x y z) =
  Point3d (x + vectorX) (y + vectorY) (z + vectorZ)


minus: Point3d -> Point3d -> Vector3d
minus (Point3d otherX otherY otherZ) (Point3d x y z) =
  Vector3d (x - otherX) (y - otherY) (z - otherZ)


hull: Point3d -> Point3d -> Box3d
hull (Point3d otherX otherY otherZ) (Point3d x y z) =
  Box3d (Scalar.hull otherX x) (Scalar.hull otherY y) (Scalar.hull otherZ z)
