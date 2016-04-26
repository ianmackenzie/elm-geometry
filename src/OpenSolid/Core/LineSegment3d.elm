module OpenSolid.Core.LineSegment3d
  ( fromEndpoints
  , endpoints
  , mapReduce
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


mapReduce: (Point3d -> a) -> (a -> a -> b) -> LineSegment3d -> b
mapReduce map reduce lineSegment =
  reduce (map lineSegment.firstEndpoint) (map lineSegment.secondEndpoint)


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
  mapReduce (Point3d.scaledAbout point scale) LineSegment3d


projectedOntoAxis: Axis3d -> LineSegment3d -> LineSegment3d
projectedOntoAxis axis =
  mapReduce (Point3d.projectedOntoAxis axis) LineSegment3d


projectedOnto: Plane3d -> LineSegment3d -> LineSegment3d
projectedOnto plane =
  mapReduce (Point3d.projectedOnto plane) LineSegment3d


projectedInto: Plane3d -> LineSegment3d -> LineSegment2d
projectedInto plane =
  mapReduce (Point3d.projectedInto plane) LineSegment2d
