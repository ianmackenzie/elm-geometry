module OpenSolid.LineSegment2d
  ( LineSegment2d
  , endpoints
  , vector
  , direction
  , normalDirection
  , squaredLength
  , length
  , transformedBy
  ) where


import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Transformation2d as Transformation2d exposing (Transformation2d)


type alias LineSegment2d =
  { firstEndpoint : Point2d
  , secondEndpoint : Point2d
  }


endpoints: LineSegment2d -> (Point2d, Point2d)
endpoints lineSegment =
  (lineSegment.firstEndpoint, lineSegment.secondEndpoint)


vector: LineSegment2d -> Vector2d
vector lineSegment =
  Point2d.minus lineSegment.secondEndpoint lineSegment.firstEndpoint


direction: LineSegment2d -> Direction2d
direction lineSegment =
  Vector2d.direction (vector lineSegment)


normalDirection: LineSegment2d -> Direction2d
normalDirection lineSegment =
  Vector2d.normalDirection (vector lineSegment)


squaredLength: LineSegment2d -> Float
squaredLength lineSegment =
  Vector2d.squaredLength (vector lineSegment)


length: LineSegment2d -> Float
length lineSegment =
  Vector2d.length (vector lineSegment)


transformedBy: Transformation2d -> LineSegment2d -> LineSegment2d
transformedBy transformation lineSegment =
  let
    firstEndpoint = transformation.ofPoint lineSegment.firstEndpoint
    secondEndpoint = transformation.ofPoint lineSegment.secondEndpoint
  in
    LineSegment2d firstEndpoint secondEndpoint
