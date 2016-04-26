module OpenSolid.Core.Point3d
  ( origin
  , fromComponents
  , components
  , squaredDistanceTo
  , distanceTo
  , vectorTo
  , distanceAlong
  , squaredDistanceToAxis
  , distanceToAxis
  , signedDistanceFrom
  , scaledAbout
  , rotatedAbout
  , relativeTo
  , placedIn
  , projectedOntoAxis
  , projectedOnto
  , projectedInto
  , plus
  , minus
  , hull
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Scalar as Scalar
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Direction3d as Direction3d


origin: Point3d
origin =
  Point3d 0 0 0


fromComponents: (Float, Float, Float) -> Point3d
fromComponents (x, y, z) =
  Point3d x y z


components: Point3d -> (Float, Float, Float)
components (Point3d x y z) =
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


vectorFrom: Point3d -> Point3d -> Vector3d
vectorFrom (Point3d x2 y2 z2) (Point3d x1 y1 z1) =
  Vector3d (x1 - x2) (y1 - y2) (z1 - z2)


distanceAlong: Axis3d -> Point3d -> Float
distanceAlong axis =
  vectorFrom axis.originPoint >> Vector3d.componentIn axis.direction


squaredDistanceToAxis: Axis3d -> Point3d -> Float
squaredDistanceToAxis axis =
  let
    directionVector = Direction3d.vector axis.direction
  in
    vectorFrom axis.originPoint >> Vector3d.cross directionVector >> Vector3d.squaredLength


distanceToAxis: Axis3d -> Point3d -> Float
distanceToAxis axis =
  squaredDistanceToAxis axis >> sqrt


signedDistanceFrom: Plane3d -> Point3d -> Float
signedDistanceFrom plane =
  vectorFrom plane.originPoint >> Vector3d.componentIn plane.normalDirection


scaledAbout: Point3d -> Float -> Point3d -> Point3d
scaledAbout centerPoint scale =
  vectorFrom centerPoint >> Vector3d.times scale >> Vector3d.addedTo centerPoint


rotatedAbout: Axis3d -> Float -> Point3d -> Point3d
rotatedAbout axis angle =
  let
    rotateVector = Vector3d.rotatedAbout axis.direction angle
  in
    vectorFrom axis.originPoint >> rotateVector >> Vector3d.addedTo axis.originPoint


relativeTo: Frame3d -> Point3d -> Point3d
relativeTo frame =
  let
    localizeVector = Vector3d.relativeTo frame
  in
    vectorFrom frame.originPoint >> localizeVector >> (\(Vector3d x y z) -> Point3d x y z)


placedIn: Frame3d -> Point3d -> Point3d
placedIn frame =
  let
    globalizeVector = Vector3d.placedIn frame
  in
    (\(Point3d x y z) -> Vector3d x y z) >> globalizeVector >> Vector3d.addedTo frame.originPoint


projectedOntoAxis: Axis3d -> Point3d -> Point3d
projectedOntoAxis axis =
  vectorFrom axis.originPoint >>
    Vector3d.projectionIn axis.direction >>
      Vector3d.addedTo axis.originPoint


projectedOnto: Plane3d -> Point3d -> Point3d
projectedOnto plane point =
  let
    distance = signedDistanceFrom plane point
    displacement = Direction3d.times distance plane.normalDirection
  in
    minus displacement point


projectedInto: Plane3d -> Point3d -> Point2d
projectedInto plane =
  vectorFrom plane.originPoint >> Vector3d.projectedInto plane >> (\(Vector2d x y) -> Point2d x y)


plus: Vector3d -> Point3d -> Point3d
plus (Vector3d vx vy vz) (Point3d px py pz) =
  Point3d (px + vx) (py + vy) (pz + vz)


minus: Vector3d -> Point3d -> Point3d
minus (Vector3d vx vy vz) (Point3d px py pz) =
  Point3d (px - vx) (py - vy) (pz - vz)


hull: Point3d -> Point3d -> Bounds3d
hull (Point3d x2 y2 z2) (Point3d x1 y1 z1) =
  Bounds3d (Scalar.hull x1 x2) (Scalar.hull y1 y2) (Scalar.hull z1 z2)
