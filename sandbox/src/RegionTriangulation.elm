module RegionTriangulation exposing (main)

import Angle exposing (Angle)
import Arc2d exposing (Arc2d)
import Circle2d exposing (Circle2d)
import Color
import Curve2d exposing (Curve2d)
import Drawing2d
import Html exposing (Html)
import Length exposing (Meters)
import LineSegment2d exposing (LineSegment2d)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Polygon2d
import Quantity exposing (Quantity)
import Rectangle2d exposing (Rectangle2d)
import Region2d exposing (Region2d)
import Triangle2d
import TriangularMesh exposing (TriangularMesh)


type DrawingCoordinates
    = DrawingCoordinates


region : Region2d Meters DrawingCoordinates
region =
    let
        p1 =
            Point2d.origin

        p2 =
            Point2d.centimeters 7 0

        p3 =
            Point2d.centimeters 7 2

        p4 =
            Point2d.centimeters 5 4

        p5 =
            Point2d.centimeters 0 4
    in
    Region2d.withHoles
        [ [ Curve2d.circle (Circle2d.withRadius (Length.centimeters 1) (Point2d.centimeters 2 2)) ]
        , [ Curve2d.circle (Circle2d.withRadius (Length.centimeters 1) (Point2d.centimeters 5 2)) ]
        ]
        [ Curve2d.lineSegment (LineSegment2d.from p1 p2)
        , Curve2d.lineSegment (LineSegment2d.from p2 p3)
        , Curve2d.arc (Arc2d.from p3 p4 (Angle.degrees 90))
        , Curve2d.lineSegment (LineSegment2d.from p4 p5)
        , Curve2d.lineSegment (LineSegment2d.from p5 p1)
        ]


main : Html msg
main =
    let
        resolution =
            Pixels.float 100 |> Quantity.per Length.centimeter

        polygon =
            Region2d.approximate (Pixels.float 0.5) (Region2d.at resolution region)

        mesh =
            Polygon2d.triangulate polygon

        lineSegments =
            List.map LineSegment2d.fromEndpoints (TriangularMesh.edgeVertices mesh)

        triangles =
            List.map Triangle2d.fromVertices (TriangularMesh.faceVertices mesh)
    in
    Drawing2d.toHtml
        { size = Drawing2d.fixed
        , viewBox = Rectangle2d.from (Point2d.pixels -50 -50) (Point2d.pixels 750 750)
        }
        []
        [ Drawing2d.group [ Drawing2d.fillColor Color.lightGrey, Drawing2d.noBorder ]
            (List.map (Drawing2d.triangle []) triangles)
        , Drawing2d.group [ Drawing2d.strokeColor Color.darkGrey ]
            (List.map (Drawing2d.lineSegment []) lineSegments)
        ]
