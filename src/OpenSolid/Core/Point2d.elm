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
  , hull
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Scalar as Scalar
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Direction2d as Direction2d


origin: Point2d
origin =
  Point2d 0 0


polar: Float -> Float -> Point2d
polar radius angle =
  Point2d (radius * cos angle) (radius * sin angle)


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
placedOntoPlane plane (Point2d x y) =
  let
    (Point3d px py pz) = plane.originPoint
    (Vector3d vx vy vz) = Vector2d.placedOntoPlane plane (Vector2d x y)
  in
    Point3d (px + vx) (py + vy) (pz + vz)


plus: Vector2d -> Point2d -> Point2d
plus (Vector2d vectorX vectorY) (Point2d x y) =
  Point2d (x + vectorX) (y + vectorY)


minus: Point2d -> Point2d -> Vector2d
minus (Point2d otherX otherY) (Point2d x y) =
  Vector2d (x - otherX) (y - otherY)


hull: Point2d -> Point2d -> Bounds2d
hull (Point2d otherX otherY) (Point2d x y) =
  Bounds2d (Scalar.hull otherX x) (Scalar.hull otherY y)
