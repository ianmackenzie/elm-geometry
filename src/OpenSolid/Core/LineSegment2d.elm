module OpenSolid.Core.LineSegment2d
  ( endpoints
  , vector
  , direction
  , normalDirection
  , squaredLength
  , length
  , transformedBy
  , projectedOntoAxis
  , placedOntoPlane
  ) where


import OpenSolid.Core exposing (..)
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Point2d as Point2d


endpoints: LineSegment2d -> (Point2d, Point2d)
endpoints lineSegment =
  (lineSegment.firstEndpoint, lineSegment.secondEndpoint)


vector: LineSegment2d -> Vector2d
vector lineSegment =
  Point2d.minus lineSegment.firstEndpoint lineSegment.secondEndpoint


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


transformedBy: Transformation2d -> LineSegment2d -> LineSegment2d
transformedBy transformation lineSegment =
  let
    transformPoint = Point2d.transformedBy transformation
    firstEndpoint = transformPoint lineSegment.firstEndpoint
    secondEndpoint = transformPoint lineSegment.secondEndpoint
  in
    LineSegment2d firstEndpoint secondEndpoint


projectedOntoAxis: Axis2d -> LineSegment2d -> LineSegment2d
projectedOntoAxis axis lineSegment =
  let
    projectPoint = Point2d.projectedOntoAxis axis
    firstEndpoint = projectPoint lineSegment.firstEndpoint
    secondEndpoint = projectPoint lineSegment.secondEndpoint
  in
    LineSegment2d firstEndpoint secondEndpoint


placedOntoPlane: Plane3d -> LineSegment2d -> LineSegment3d
placedOntoPlane plane lineSegment =
  let
    placePoint = Point2d.placedOntoPlane plane
    firstEndpoint = placePoint lineSegment.firstEndpoint
    secondEndpoint = placePoint lineSegment.secondEndpoint
  in
    LineSegment3d firstEndpoint secondEndpoint
