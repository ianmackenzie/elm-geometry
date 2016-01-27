module OpenSolid.Core.LineSegment2d
  ( endpoints
  , mapReduce
  , vector
  , direction
  , normalDirection
  , squaredLength
  , length
  , scaledAbout
  , transformedBy
  , projectedOntoAxis
  , placedOntoPlane
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Point2d as Point2d


endpoints: LineSegment2d -> (Point2d, Point2d)
endpoints (LineSegment2d firstEndpoint secondEndpoint) =
  (firstEndpoint, secondEndpoint)


mapReduce: (Point2d -> a) -> (a -> a -> b) -> LineSegment2d -> b
mapReduce map reduce (LineSegment2d firstEndpoint secondEndpoint) =
  reduce (map firstEndpoint) (map secondEndpoint)


vector: LineSegment2d -> Vector2d
vector (LineSegment2d firstEndpoint secondEndpoint) =
  Point2d.minus firstEndpoint secondEndpoint


direction: LineSegment2d -> Direction2d
direction =
  vector >> Vector2d.direction


normalDirection: LineSegment2d -> Direction2d
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


transformedBy: Transformation2d -> LineSegment2d -> LineSegment2d
transformedBy transformation =
  mapReduce (Point2d.transformedBy transformation) LineSegment2d


projectedOntoAxis: Axis2d -> LineSegment2d -> LineSegment2d
projectedOntoAxis axis =
  mapReduce (Point2d.projectedOntoAxis axis) LineSegment2d


placedOntoPlane: Plane3d -> LineSegment2d -> LineSegment3d
placedOntoPlane plane =
  mapReduce (Point2d.placedOntoPlane plane) LineSegment3d
