import Html exposing (Html, div, text)
import Html.Attributes exposing (id, style)
import Graphics.Element
import Graphics.Collage exposing (Path, path, collage, traced, dashed)
import Color exposing (blue)
import OpenSolid.Interval as Interval exposing (Interval)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import OpenSolid.Transformation2d as Transformation2d exposing (Transformation2d)

line: String -> a -> Html
line label value =
  div [] [text (label ++ ": " ++ (toString value))]

transform: LineSegment2d -> LineSegment2d
transform =
  LineSegment2d.transformedBy (Transformation2d.rotation Point2d.origin (degrees 45))

main =
  let
    intervalWidth = Interval.width (Interval 2 3)
    vectorLength = Vector2d.length (Vector2d 1 1)
    pointDifference = Point2d.minus Point2d.origin (Point2d 1 2)
    rotation = Transformation2d.rotation Point2d.origin (degrees 45)
    lineSegment = LineSegment2d (Point2d 1 0) (Point2d 2 0)
    transformedSegment = LineSegment2d.transformedBy rotation lineSegment
    transformedSegment2 = transform lineSegment
    mixedDotProduct = Vector2d.dot Direction2d.x (Vector2d 2 3)
    rotatedDirection = Direction2d.transformedBy rotation Direction2d.x
    angledDotProduct = Vector2d.dot rotatedDirection (Vector2d 2 3)
  in
    div []
      [ line "Interval width" intervalWidth
      , line "Vector length" vectorLength
      , line "Point difference" pointDifference
      , line "Transformed line segment" transformedSegment
      , line "Transformed line segment 2" transformedSegment2
      , line "Mixed dot product" mixedDotProduct
      , line "Rotated direction" rotatedDirection
      , line "Angled dot product" angledDotProduct
      , testImage
      ]

toPath: LineSegment2d -> Path
toPath lineSegment =
  path [Point2d.components lineSegment.firstEndpoint, Point2d.components lineSegment.secondEndpoint]

testImage: Html
testImage =
  let
    centerPoint = Point2d 0 0
    lineSegment = LineSegment2d (Point2d 100 0) (Point2d 250 0)
    angle = degrees 15
    forms =
      List.map
        ( (*) angle >> Transformation2d.rotation centerPoint >>
          (\rotation -> LineSegment2d.transformedBy rotation lineSegment) >>
          toPath >> traced (dashed blue) )
        [0..18]
  in
    div [] [Html.fromElement (collage 500 500 forms)]
