import Html exposing (Html)
import Time exposing (Time)
import String
import Signal exposing (Signal)
import Debug
import OpenSolid.Core exposing (..)
import OpenSolid.Core.Axis2d as Axis2d
import OpenSolid.Core.Axis3d as Axis3d
import OpenSolid.Core.Box2d as Box2d
import OpenSolid.Core.Box3d as Box3d
import OpenSolid.Core.Direction2d as Direction2d
import OpenSolid.Core.Direction3d as Direction3d
import OpenSolid.Core.Frame2d as Frame2d
import OpenSolid.Core.Frame3d as Frame3d
import OpenSolid.Core.Interval as Interval
import OpenSolid.Core.LineSegment2d as LineSegment2d
import OpenSolid.Core.Plane3d as Plane3d
import OpenSolid.Core.Point2d as Point2d
import OpenSolid.Core.Point3d as Point3d
import OpenSolid.Core.Transformation2d as Transformation2d
import OpenSolid.Core.Transformation3d as Transformation3d
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Vector3d as Vector3d


type alias State =
  { currentTime: Time
  }


initialState: State
initialState =
  { currentTime = 0
  }


type Event
  = CurrentTime Time


update: Event -> State -> State
update event state =
  case event of
    CurrentTime currentTime ->
      { currentTime = currentTime
      }


line: String -> a -> Html
line label value =
  Html.div [] [Html.text (label ++ ": " ++ toString value)]


timeString: Time -> String
timeString time =
  let
    seconds = round (Time.inSeconds time)
    minutes = seconds // 60
    hours = minutes // 60
    format length number =
      String.padLeft length '0' (toString number)
    secondsString = format 2 (seconds % 60)
    minutesString = format 2 (minutes % 60)
    hoursString = format 1 ((hours + 11) % 24)
  in
    String.join ":" [hoursString, minutesString, secondsString]


view: State -> Html
view state =
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
    Html.div []
      [ line "Interval width" intervalWidth
      , line "Vector length" vectorLength
      , line "Point difference" pointDifference
      , line "Transformed line segment" transformedSegment
      , line "Mixed dot product" mixedDotProduct
      , line "Rotated direction" rotatedDirection
      , line "Angled dot product" angledDotProduct
      , line "Current time" (timeString state.currentTime)
      ]


main: Signal Html
main =
  let
    timeSignal = Signal.map CurrentTime (Time.every Time.second)
  in
    Signal.foldp update initialState timeSignal |> Signal.map view
