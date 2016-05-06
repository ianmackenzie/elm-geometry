module OpenSolid.Core.Point2d
  ( origin
  , fromComponents
  , fromPolarComponents
  , components
  , polarComponents
  , squaredDistanceTo
  , distanceTo
  , vectorTo
  , vectorFrom
  , distanceAlong
  , signedDistanceFrom
  , scaledAbout
  , rotatedAbout
  , mirroredAbout
  , relativeTo
  , placedIn
  , projectedOnto
  , placedOnto
  , plus
  , minus
  , hull
  , hullOf
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Scalar as Scalar
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Direction2d as Direction2d
import OpenSolid.Core.BoundingBox2d as BoundingBox2d


origin: Point2d
origin =
  Point2d 0 0


fromComponents: (Float, Float) -> Point2d
fromComponents (x, y) =
  Point2d x y


fromPolarComponents: (Float, Float) -> Point2d
fromPolarComponents =
  fromPolar >> fromComponents


components: Point2d -> (Float, Float)
components (Point2d x y) =
  (x, y)


polarComponents: Point2d -> (Float, Float)
polarComponents =
  components >> toPolar


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


distanceAlong: Axis2d -> Point2d -> Float
distanceAlong axis =
  vectorFrom axis.originPoint >> Vector2d.componentIn axis.direction


signedDistanceFrom: Axis2d -> Point2d -> Float
signedDistanceFrom axis =
  let
    normalDirection = Direction2d.normalDirection axis.direction
  in
    vectorFrom axis.originPoint >> Vector2d.componentIn normalDirection


scaledAbout: Point2d -> Float -> Point2d -> Point2d
scaledAbout centerPoint scale =
  vectorFrom centerPoint >> Vector2d.times scale >> Vector2d.addedTo centerPoint


rotatedAbout: Point2d -> Float -> Point2d -> Point2d
rotatedAbout centerPoint angle =
  let
    rotateVector = Vector2d.rotatedBy angle
  in
    vectorFrom centerPoint >> rotateVector >> Vector2d.addedTo centerPoint


mirroredAbout: Axis2d -> Point2d -> Point2d
mirroredAbout axis =
  let
    mirrorVector = Vector2d.mirroredAbout axis.direction
  in
    vectorFrom axis.originPoint >> mirrorVector >> Vector2d.addedTo axis.originPoint


relativeTo: Frame2d -> Point2d -> Point2d
relativeTo frame =
  let
    localizeVector = Vector2d.relativeTo frame
  in
    vectorFrom frame.originPoint >> localizeVector >> (\(Vector2d x y) -> Point2d x y)


placedIn: Frame2d -> Point2d -> Point2d
placedIn frame =
  let
    globalizeVector = Vector2d.placedIn frame
  in
    (\(Point2d x y) -> Vector2d x y) >> globalizeVector >> Vector2d.addedTo frame.originPoint


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


hull: Point2d -> Point2d -> BoundingBox2d
hull (Point2d x2 y2) (Point2d x1 y1) =
  BoundingBox2d (Scalar.hull x2 x1) (Scalar.hull y2 y1)


hullOf: List Point2d -> BoundingBox2d
hullOf =
  List.map BoundingBox2d.singleton >> BoundingBox2d.hullOf
