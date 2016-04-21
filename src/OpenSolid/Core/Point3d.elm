module OpenSolid.Core.Point3d
  ( origin
  , fromTuple
  , toTuple
  , squaredDistanceTo
  , distanceTo
  , vectorTo
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
import OpenSolid.Core.Interval as Interval
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Direction3d as Direction3d


toVector3d: Direction3d -> Vector3d
toVector3d (Direction3d x y z) =
  Vector3d x y z


origin: Point3d
origin =
  Point3d 0 0 0


fromTuple: (Float, Float, Float) -> Point3d
fromTuple (x, y, z) =
  Point3d x y z


toTuple: Point3d -> (Float, Float, Float)
toTuple (Point3d x y z) =
  (x, y, z)


squaredDistanceTo: Point3d -> Point3d -> Float
squaredDistanceTo other =
  vectorTo other >> Vector3d.squaredLength


distanceTo: Point3d -> Point3d -> Float
distanceTo other =
  squaredDistanceTo other >> sqrt


vectorTo: Point3d -> Point3d -> Vector3d
vectorTo (Point3d x2 y2 z2) (Point3d x1 y1 z1) =
  Vector3d (x2 - x1) (y2 - y1) (z2 - z1)


squaredDistanceToAxis: Axis3d -> Point3d -> Float
squaredDistanceToAxis axis =
  vectorTo axis.originPoint >> Vector3d.cross (toVector3d axis.direction) >> Vector3d.squaredLength


distanceToAxis: Axis3d -> Point3d -> Float
distanceToAxis axis =
  squaredDistanceToAxis axis >> sqrt


distanceToPlane: Plane3d -> Point3d -> Float
distanceToPlane plane =
  vectorTo plane.originPoint >> Vector3d.componentIn plane.normalDirection


scaledAbout: Point3d -> Float -> Point3d -> Point3d
scaledAbout originPoint scale point =
  let
    displacement = vectorTo point originPoint
  in
    plus (Vector3d.times scale displacement) originPoint


transformedBy: Transformation3d -> Point3d -> Point3d
transformedBy (_, transformPoint) =
  transformPoint


projectedOntoAxis: Axis3d -> Point3d -> Point3d
projectedOntoAxis axis point =
  let
    displacement = vectorTo point axis.originPoint
    projectedDisplacement = Vector3d.projectedOntoAxis axis displacement
  in
    plus projectedDisplacement axis.originPoint


projectedOntoPlane: Plane3d -> Point3d -> Point3d
projectedOntoPlane plane point =
  let
    distance = distanceToPlane plane point
    normalDisplacement = Direction3d.times distance plane.normalDirection
  in
    plus normalDisplacement point


projectedIntoPlane: Plane3d -> Point3d -> Point2d
projectedIntoPlane plane point =
  let
    (Vector2d x y) = Vector3d.projectedIntoPlane plane (vectorTo point plane.originPoint)
  in
    Point2d x y


plus: Vector3d -> Point3d -> Point3d
plus (Vector3d vx vy vz) (Point3d px py pz) =
  Point3d (px + vx) (py + vy) (pz + vz)


minus: Vector3d -> Point3d -> Point3d
minus (Vector3d vx vy vz) (Point3d px py pz) =
  Point3d (px - vx) (py - vy) (pz - vz)


hull: Point3d -> Point3d -> Bounds3d
hull (Point3d x2 y2 z2) (Point3d x1 y1 z1) =
  Bounds3d (Interval.hullOf x1 x2) (Interval.hullOf y1 y2) (Interval.hullOf z1 z2)
