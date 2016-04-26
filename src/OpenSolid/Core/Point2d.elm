module OpenSolid.Core.Point2d
  ( origin
  , polar
  , fromComponents
  , components
  , squaredDistanceTo
  , distanceTo
  , vectorTo
  , vectorFrom
  , distanceAlongAxis
  , distanceToAxis
  , scaledAbout
  , rotatedAbout
  , projectedOnto
  , placedOnto
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


fromComponents: (Float, Float) -> Point2d
fromComponents (x, y) =
  Point2d x y


components: Point2d -> (Float, Float)
components (Point2d x y) =
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
scaledAbout centerPoint scale =
  vectorFrom centerPoint >> Vector2d.times scale >> Vector2d.addedTo centerPoint


rotatedAbout: Point2d -> Float -> Point2d -> Point2d
rotatedAbout centerPoint angle =
  let
    rotateVector = Vector2d.rotatedBy angle
  in
    vectorFrom centerPoint >> rotateVector >> Vector2d.addedTo centerPoint


projectedOnto: Axis2d -> Point2d -> Point2d
projectedOnto axis =
  vectorFrom axis.originPoint >> Vector2d.projectedOnto axis >> Vector2d.addedTo axis.originPoint


placedOnto: Plane3d -> Point2d -> Point3d
placedOnto plane (Point2d x y) =
  let
    (Vector3d vx vy vz) = Vector2d.placedOnto plane (Vector2d x y)
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
  Bounds2d (Scalar.hull x2 x1) (Scalar.hull y2 y1)
