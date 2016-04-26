module OpenSolid.Core.LineSegment2d
  ( fromEndpoints
  , endpoints
  , mapReduce
  , vector
  , direction
  , normalDirection
  , squaredLength
  , length
  , scaledAbout
  , rotatedAbout
  , projectedOnto
  , placedOnto
  ) where


import Maybe exposing (..)
import OpenSolid.Core exposing (..)
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Point2d as Point2d


fromEndpoints: (Point2d, Point2d) -> LineSegment2d
fromEndpoints (firstEndpoint, secondEndpoint) =
  LineSegment2d firstEndpoint secondEndpoint


endpoints: LineSegment2d -> (Point2d, Point2d)
endpoints lineSegment =
  (lineSegment.firstEndpoint, lineSegment.secondEndpoint)


mapReduce: (Point2d -> a) -> (a -> a -> b) -> LineSegment2d -> b
mapReduce map reduce lineSegment =
  reduce (map lineSegment.firstEndpoint) (map lineSegment.secondEndpoint)


vector: LineSegment2d -> Vector2d
vector lineSegment =
  Point2d.vectorTo lineSegment.secondEndpoint lineSegment.firstEndpoint


direction: LineSegment2d -> Maybe Direction2d
direction =
  vector >> Vector2d.direction


normalDirection: LineSegment2d -> Maybe Direction2d
normalDirection =
  vector >> Vector2d.normalDirection


squaredLength: LineSegment2d -> Float
squaredLength =
  vector >> Vector2d.squaredLength


length: LineSegment2d -> Float
length =
  vector >> Vector2d.length


scaledAbout: Point2d -> Float -> LineSegment2d -> LineSegment2d
scaledAbout point scale =
  mapReduce (Point2d.scaledAbout point scale) LineSegment2d


rotatedAbout: Point2d -> Float -> LineSegment2d -> LineSegment2d
rotatedAbout centerPoint angle =
  mapReduce (Point2d.rotatedAbout centerPoint angle) LineSegment2d


projectedOnto: Axis2d -> LineSegment2d -> LineSegment2d
projectedOnto axis =
  mapReduce (Point2d.projectedOnto axis) LineSegment2d


placedOnto: Plane3d -> LineSegment2d -> LineSegment3d
placedOnto plane =
  mapReduce (Point2d.placedOnto plane) LineSegment3d
