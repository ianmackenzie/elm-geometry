module DistanceAlongAxis2d exposing (drawProjection)

import Angle exposing (Angle)
import Axis2d exposing (Axis2d)
import Axis3d exposing (Axis3d)
import BoundingBox2d exposing (BoundingBox2d)
import BoundingBox3d exposing (BoundingBox3d)
import Browser
import Browser.Events
import Circle3d
import Color exposing (Color)
import Direction2d exposing (Direction2d)
import Direction3d exposing (perpendicularTo)
import Drawing2d
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import LineSegment2d exposing (LineSegment2d)
import LineSegment3d exposing (LineSegment3d)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity.Interval as Interval exposing (Interval)
import Random exposing (Generator)
import Random2d
import Rectangle2d exposing (Rectangle2d)


drawProjection : Axis2d Pixels coordinates -> Interval Float Pixels -> Drawing2d.Element Pixels coordinates event
drawProjection axis interval =
    let
        perpendicularDirection =
            Direction2d.perpendicularTo (Axis2d.direction axis)

        minAxis =
            Axis2d.through (Point2d.along axis (Interval.minValue interval))
                perpendicularDirection

        maxAxis =
            Axis2d.through (Point2d.along axis (Interval.maxValue interval))
                perpendicularDirection
    in
    Drawing2d.group []
        [ drawAxis Color.blue axis
        , drawAxis Color.green minAxis
        , drawAxis Color.red maxAxis
        ]


drawAxis : Color -> Axis2d Pixels coordinates -> Drawing2d.Element Pixels coordinates event
drawAxis color axis =
    let
        originPoint =
            Axis2d.originPoint axis
    in
    Drawing2d.group []
        [ Drawing2d.lineSegment [ Drawing2d.strokeColor color ]
            (LineSegment2d.along axis (Pixels.float -1000) (Pixels.float 1000))
        ]
