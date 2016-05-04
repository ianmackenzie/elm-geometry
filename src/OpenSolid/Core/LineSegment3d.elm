module OpenSolid.Core.LineSegment3d
  ( fromEndpoints
  , endpoints
  , map
  , mapTo
  , vector
  , direction
  , normalDirection
  , squaredLength
  , length
  , scaledAbout
  , projectedOntoAxis
  , projectedOnto
  , projectedInto
  ) where


import Maybe exposing (..)
import OpenSolid.Core exposing (..)
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Point3d as Point3d


fromEndpoints: (Point3d, Point3d) -> LineSegment3d
fromEndpoints (firstEndpoint, secondEndpoint) =
  LineSegment3d firstEndpoint secondEndpoint


endpoints: LineSegment3d -> (Point3d, Point3d)
endpoints lineSegment =
  (lineSegment.firstEndpoint, lineSegment.secondEndpoint)


map: (Point3d -> Point3d) -> LineSegment3d -> LineSegment3d
map =
  mapTo LineSegment3d


mapTo: (a -> a -> b) -> (Point3d -> a) -> LineSegment3d -> b
mapTo constructor map lineSegment =
  constructor (map lineSegment.firstEndpoint) (map lineSegment.secondEndpoint)


vector: LineSegment3d -> Vector3d
vector lineSegment =
  Point3d.vectorTo lineSegment.secondEndpoint lineSegment.firstEndpoint


direction: LineSegment3d -> Maybe Direction3d
direction =
  vector >> Vector3d.direction


normalDirection: LineSegment3d -> Maybe Direction3d
normalDirection =
  vector >> Vector3d.normalDirection


squaredLength: LineSegment3d -> Float
squaredLength =
  vector >> Vector3d.squaredLength


length: LineSegment3d -> Float
length =
  vector >> Vector3d.length


scaledAbout: Point3d -> Float -> LineSegment3d -> LineSegment3d
scaledAbout point scale =
  let
    scalePoint = Point3d.scaledAbout point scale
  in
    map scalePoint


projectedOntoAxis: Axis3d -> LineSegment3d -> LineSegment3d
projectedOntoAxis axis =
  let
    projectPoint = Point3d.projectedOntoAxis axis
  in
    map projectPoint


projectedOnto: Plane3d -> LineSegment3d -> LineSegment3d
projectedOnto plane =
  let
    projectPoint = Point3d.projectedOnto plane
  in
    map projectPoint


projectedInto: Plane3d -> LineSegment3d -> LineSegment2d
projectedInto plane =
  let
    projectPoint = Point3d.projectedInto plane
  in
    mapTo LineSegment2d projectPoint
