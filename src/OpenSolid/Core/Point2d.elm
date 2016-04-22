module OpenSolid.Core.Point2d
  ( origin
  , polar
  , fromTuple
  , toTuple
  , squaredDistanceTo
  , distanceTo
  , vectorTo
  , vectorFrom
  , distanceAlongAxis
  , distanceToAxis
  , scaledAbout
  , rotatedAbout
  , projectedOntoAxis
  , placedOntoPlane
  , plus
  , minus
  , hull
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Interval as Interval
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Direction2d as Direction2d


origin: Point2d
origin =
  Point2d 0 0


polar: Float -> Float -> Point2d
polar radius angle =
  Point2d (radius * cos angle) (radius * sin angle)


fromTuple: (Float, Float) -> Point2d
fromTuple (x, y) =
  Point2d x y


toTuple: Point2d -> (Float, Float)
toTuple (Point2d x y) =
  (x, y)


squaredDistanceTo: Point2d -> Point2d -> Float
squaredDistanceTo other =
  vectorTo other >> Vector2d.squaredLength


distanceTo: Point2d -> Point2d -> Float
distanceTo other =
  squaredDistanceTo other >> sqrt


vectorTo: Point2d -> Point2d -> Vector2d
vectorTo (Point2d x2 y2) (Point2d x1 y1) =
  Vector2d (x2 - x1) (y2 - y1)


vectorFrom: Point2d -> Point2d -> Vector2d
vectorFrom (Point2d x2 y2) (Point2d x1 y1) =
  Vector2d (x1 - x2) (y1 - y2)


distanceAlongAxis: Axis2d -> Point2d -> Float
distanceAlongAxis axis point =
  Vector2d.componentIn axis.direction (vectorTo point axis.originPoint)


distanceToAxis: Axis2d -> Point2d -> Float
distanceToAxis axis =
  vectorTo axis.originPoint >> Vector2d.componentIn (Direction2d.normalDirection axis.direction)


scaledAbout: Point2d -> Float -> Point2d -> Point2d
scaledAbout originPoint scale point =
  let
    displacement = vectorTo point originPoint
  in
    plus (Vector2d.times scale displacement) originPoint


rotatedAbout: Point2d -> Float -> Point2d -> Point2d
rotatedAbout centerPoint angle =
  let
    rotateVector = Vector2d.rotatedBy angle
  in
    vectorFrom centerPoint >> rotateVector >> Vector2d.addedTo centerPoint


projectedOntoAxis: Axis2d -> Point2d -> Point2d
projectedOntoAxis axis point =
  let
    displacement = vectorTo point axis.originPoint
    projectedDisplacement = Vector2d.projectedOntoAxis axis displacement
  in
    plus projectedDisplacement axis.originPoint


placedOntoPlane: Plane3d -> Point2d -> Point3d
placedOntoPlane plane (Point2d x y) =
  let
    (Vector3d vx vy vz) = Vector2d.placedOntoPlane plane (Vector2d x y)
    (Point3d px py pz) = plane.originPoint
  in
    Point3d (px + vx) (py + vy) (pz + vz)


plus: Vector2d -> Point2d -> Point2d
plus (Vector2d vx vy) (Point2d px py) =
  Point2d (px + vx) (py + vy)


minus: Vector2d -> Point2d -> Point2d
minus (Vector2d vx vy) (Point2d px py) =
  Point2d (px - vx) (py - vy)


hull: Point2d -> Point2d -> Bounds2d
hull (Point2d x2 y2) (Point2d x1 y1) =
  Bounds2d (Interval.hullOf x1 x2) (Interval.hullOf y1 y2)
