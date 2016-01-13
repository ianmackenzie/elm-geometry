import Html exposing (Html, div, text)
import Time exposing (Time, fps)
import Signal exposing (Signal)
import AnimationFrame
import Svg.Attributes exposing (stroke, strokeWidth)
import Debug
import OpenSolid.Interval as Interval exposing (Interval)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import OpenSolid.Transformation2d as Transformation2d exposing (Transformation2d)
import OpenSolid.Box2d as Box2d exposing (Box2d)
import OpenSolid.Svg as Svg exposing (svg)


line: String -> a -> Html
line label value =
  div [] [text (label ++ ": " ++ toString value)]


lines: Html
lines =
  let
    intervalWidth = Interval.width (Interval 2 3)
    vectorLength = Vector2d.length (Vector2d 1 1)
    pointDifference = Point2d.minus Point2d.origin (Point2d 1 2)
    rotation = Transformation2d.rotationAbout Point2d.origin (degrees 45)
    lineSegment = LineSegment2d (Point2d 1 0) (Point2d 2 0)
    transformedSegment = LineSegment2d.transformedBy rotation lineSegment
    mixedDotProduct = Vector2d.dot Direction2d.x (Vector2d 2 3)
    rotatedDirection = Direction2d.transformedBy rotation Direction2d.x
    angledDotProduct = Vector2d.dot rotatedDirection (Vector2d 2 3)
  in
    div []
      [ line "Interval width" intervalWidth
      , line "Vector length" vectorLength
      , line "Point difference" pointDifference
      , line "Transformed line segment" transformedSegment
      , line "Mixed dot product" mixedDotProduct
      , line "Rotated direction" rotatedDirection
      , line "Angled dot product" angledDotProduct
      ]


angularSpeed: Float
angularSpeed =
  -pi / 16


type alias State =
  { angle: Float
  , frameRate: Float
  , elapsedTime: Float
  , frameCount: Int
  }


initialState: State
initialState =
  { angle = 0.0
  , frameRate = 0.0
  , elapsedTime = 0.0
  , frameCount = 0
  }


wrapAngle: Float -> Float
wrapAngle angle =
  if angle < -pi then
    angle + 2 * pi
  else if angle > pi then
    angle - 2 * pi
  else
    angle


update: Time -> State -> State
update deltaTime state =
  let
    deltaInSeconds = Time.inSeconds deltaTime
    angle = wrapAngle (state.angle + angularSpeed * deltaInSeconds)
    frameCount = state.frameCount + 1
    elapsedTime = state.elapsedTime + deltaInSeconds
  in
    if elapsedTime >= 0.5 then
      State angle (frameCount / elapsedTime) 0.0 0
    else
      State angle state.frameRate elapsedTime frameCount


lineSegments: List LineSegment2d
lineSegments =
  let
    firstSegment = LineSegment2d (Point2d 5 0) (Point2d 10 0)
  in
    List.map
      ( (\index -> index * degrees 7.5) >>
        (\angle -> Transformation2d.rotationAbout Point2d.origin angle) >>
        (\rotation -> LineSegment2d.transformedBy rotation firstSegment) )
      [0..36]


view: State -> Html
view state =
  let
    rotation = Transformation2d.rotationAbout Point2d.origin state.angle
    rotatedSegments = List.map (LineSegment2d.transformedBy rotation) lineSegments
    svgElements = List.map (Svg.lineSegment [stroke "blue", strokeWidth "0.05"]) rotatedSegments

    angle = Debug.watch "angle" state.angle
    frameRate = Debug.watch "frameRate" state.frameRate
    elapsedTime = Debug.watch "elapsedTime" state.elapsedTime
    frameCount = Debug.watch "frameCount" state.frameCount
  in
    div []
      [ lines
      , svg 500 500 (Box2d (Interval -10 10) (Interval -10 10)) svgElements
      , line "Frame rate" state.frameRate
      ]


main: Signal Html
main =
  Signal.foldp update initialState AnimationFrame.frame |> Signal.map view
