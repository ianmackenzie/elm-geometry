module OpenSolid.LineSegment2d
    ( LineSegment2d(LineSegment2d)
    , endpoints
    , vector
    , direction
    , normalDirection
    , squaredLength
    , length
    , transform
    ) where


import OpenSolid.Vector2d as Vector2d exposing (Vector2d(Vector2d))
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.Point2d as Point2d exposing (Point2d(Point2d))
import OpenSolid.Transformation2d as Transformation2d exposing (Transformation2d)


type LineSegment2d =
    LineSegment2d Point2d Point2d


endpoints: LineSegment2d -> (Point2d, Point2d)
endpoints (LineSegment2d start end) =
    (start, end)


vector: LineSegment2d -> Vector2d
vector (LineSegment2d start end) =
    Point2d.difference end start


direction: LineSegment2d -> Direction2d
direction lineSegment =
    Direction2d.directionOf (vector lineSegment)


normalDirection: LineSegment2d -> Direction2d
normalDirection lineSegment =
    Direction2d.normalDirection (direction lineSegment)


squaredLength: LineSegment2d -> Float
squaredLength lineSegment =
    Vector2d.squaredLength (vector lineSegment)


length: LineSegment2d -> Float
length lineSegment =
    Vector2d.length (vector lineSegment)


transform: Transformation2d -> LineSegment2d -> LineSegment2d
transform transformation (LineSegment2d start end) =
    LineSegment2d (transformation.transformPoint start) (transformation.transformPoint end)
