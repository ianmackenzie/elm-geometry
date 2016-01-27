module OpenSolid.Core.Point2d
  ( origin
  , polar
  , xComponent
  , yComponent
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
  , hull
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Scalar as Scalar
import OpenSolid.Core.Vector2d as Vector2d


origin: Point2d
origin =
  Point2d 0 0


polar: Float -> Float -> Point2d
polar radius angle =
  Point2d (radius * cos angle) (radius * sin angle)


xComponent: Point2d -> Float
xComponent (Point2d x y) =
  x


yComponent: Point2d -> Float
yComponent (Point2d x y) =
  y


components: Point2d -> (Float, Float)
components (Point2d x y) =
  (x, y)


squaredDistanceTo: Point2d -> Point2d -> Float
squaredDistanceTo other =
  minus other >> Vector2d.squaredLength


distanceTo: Point2d -> Point2d -> Float
distanceTo other =
  squaredDistanceTo other >> sqrt


distanceAlongAxis: Axis2d -> Point2d -> Float
distanceAlongAxis (Axis2d originPoint (Direction2d vector)) =
  minus originPoint >> Vector2d.dot vector


distanceToAxis: Axis2d -> Point2d -> Float
distanceToAxis (Axis2d originPoint (Direction2d vector)) =
  minus originPoint >> (flip Vector2d.cross) vector


scaledAbout: Point2d -> Float -> Point2d -> Point2d
scaledAbout originPoint scale point =
  let
    displacement = minus originPoint point
  in
    plus (Vector2d.times scale displacement) originPoint


transformedBy: Transformation2d -> Point2d -> Point2d
transformedBy (Transformation2d transformVector transformPoint) =
  transformPoint


projectedOntoAxis: Axis2d -> Point2d -> Point2d
projectedOntoAxis axis point =
  let
    (Axis2d originPoint direction) = axis
    projectedDisplacement = Vector2d.projectedOntoAxis axis (minus originPoint point)
  in
    plus projectedDisplacement originPoint


placedOntoPlane: Plane3d -> Point2d -> Point3d
placedOntoPlane plane (Point2d x y) =
  let
    (Plane3d (Point3d px py pz) xDirection yDirection normalDirection) = plane
    (Vector3d vx vy vz) = Vector2d.placedOntoPlane plane (Vector2d x y)
  in
    Point3d (px + vx) (py + vy) (pz + vz)


plus: Vector2d -> Point2d -> Point2d
plus (Vector2d vectorX vectorY) (Point2d x y) =
  Point2d (x + vectorX) (y + vectorY)


minus: Point2d -> Point2d -> Vector2d
minus (Point2d otherX otherY) (Point2d x y) =
  Vector2d (x - otherX) (y - otherY)


hull: Point2d -> Point2d -> Box2d
hull (Point2d otherX otherY) (Point2d x y) =
  Box2d (Scalar.hull otherX x) (Scalar.hull otherY y)
