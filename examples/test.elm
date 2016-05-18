module Main (..) where

import Html exposing (Html)
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Axis2d as Axis2d
import OpenSolid.Axis3d as Axis3d
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Frame2d as Frame2d
import OpenSolid.Frame3d as Frame3d
import OpenSolid.Plane3d as Plane3d
import OpenSolid.Point2d as Point2d
import OpenSolid.Point3d as Point3d
import OpenSolid.Scalar as Scalar
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Vector3d as Vector3d


line : String -> a -> Html
line label value =
  Html.div [] [ Html.text (label ++ ": " ++ toString value) ]


main : Html
main =
  let
    vectorLength =
      Vector2d.length (Vector2d 1 1)

    pointDifference =
      Point2d.vectorTo (Point2d 1 2) Point2d.origin

    directionComponent =
      Vector2d.componentIn Direction2d.x (Vector2d 2 3)

    rotatedDirection =
      Direction2d.rotateBy (degrees 45) Direction2d.x

    angledComponent =
      Vector2d.componentIn rotatedDirection (Vector2d 2 3)

    axis =
      Axis2d Point2d.origin (Direction2d.fromAngle (degrees -45))

    referenceDirection =
      Direction2d.fromAngle (degrees 15)

    positiveAngle =
      Direction2d.angleTo rotatedDirection referenceDirection

    negativeAngle =
      Direction2d.angleTo (Direction2d.fromAngle (degrees -30)) referenceDirection

    rotatedDirection3d =
      Direction3d.rotateAbout Direction3d.z (degrees 45) Direction3d.y

    angle3d =
      Direction3d.angleTo rotatedDirection3d Direction3d.x
  in
    Html.div
      []
      [ line "Vector length" vectorLength
      , line "Point difference" pointDifference
      , line "Component in direction" directionComponent
      , line "Rotated direction" rotatedDirection
      , line "Angled component" angledComponent
      , line "Axis" axis
      , line "Positive angle" (positiveAngle / degrees 1)
      , line "Negative angle" (negativeAngle / degrees 1)
      , line "3D angle" (angle3d / degrees 1)
      ]
