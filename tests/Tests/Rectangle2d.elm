module Tests.Rectangle2d exposing
    ( containmentIsConsistent
    , verticesAreConsistent
    )

import Angle exposing (Angle)
import Axis2d exposing (Axis2d)
import Expect
import Frame2d
import Fuzz exposing (Fuzzer)
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Length exposing (Meters)
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Rectangle2d exposing (Rectangle2d)
import Test exposing (Test)
import Vector2d exposing (Vector2d)


type alias Transformation coordinates =
    { rectangle : Rectangle2d Meters coordinates -> Rectangle2d Meters coordinates
    , point : Point2d Meters coordinates -> Point2d Meters coordinates
    , lineSegment : LineSegment2d Meters coordinates -> LineSegment2d Meters coordinates
    }


rotation : Point2d Meters coordinates -> Angle -> Transformation coordinates
rotation centerPoint angle =
    { rectangle = Rectangle2d.rotateAround centerPoint angle
    , point = Point2d.rotateAround centerPoint angle
    , lineSegment = LineSegment2d.rotateAround centerPoint angle
    }


translation : Vector2d Meters coordinates -> Transformation coordinates
translation displacement =
    { rectangle = Rectangle2d.translateBy displacement
    , point = Point2d.translateBy displacement
    , lineSegment = LineSegment2d.translateBy displacement
    }


scaling : Point2d Meters coordinates -> Float -> Transformation coordinates
scaling centerPoint scale =
    { rectangle = Rectangle2d.scaleAbout centerPoint scale
    , point = Point2d.scaleAbout centerPoint scale
    , lineSegment = LineSegment2d.scaleAbout centerPoint scale
    }


mirroring : Axis2d Meters coordinates -> Transformation coordinates
mirroring axis =
    { rectangle = Rectangle2d.mirrorAcross axis
    , point = Point2d.mirrorAcross axis
    , lineSegment = LineSegment2d.mirrorAcross axis
    }


transformationFuzzer : Fuzzer (Transformation coordinates)
transformationFuzzer =
    Fuzz.oneOf
        [ Fuzz.map2 rotation Fuzz.point2d Fuzz.angle
        , Fuzz.map translation Fuzz.vector2d
        , Fuzz.map2 scaling Fuzz.point2d Fuzz.scale
        , Fuzz.map mirroring Fuzz.axis2d
        ]


containmentIsConsistent : Test
containmentIsConsistent =
    Test.fuzz3
        transformationFuzzer
        Fuzz.rectangle2d
        Fuzz.point2d
        "Rectangle/point containment is consistent through transformation"
        (\transformation rectangle point ->
            let
                initialContainment =
                    Rectangle2d.contains point rectangle

                transformedPoint =
                    transformation.point point

                transformedRectangle =
                    transformation.rectangle rectangle

                finalContainment =
                    Rectangle2d.contains transformedPoint transformedRectangle
            in
            finalContainment |> Expect.equal initialContainment
        )


verticesAreConsistent : Test
verticesAreConsistent =
    let
        testVertex description accessor =
            Test.fuzz2
                transformationFuzzer
                Fuzz.rectangle2d
                description
                (\transformation rectangle ->
                    let
                        vertex =
                            accessor rectangle

                        transformedRectangle =
                            transformation.rectangle rectangle

                        transformedVertex =
                            transformation.point vertex

                        vertexOfTransformed =
                            accessor transformedRectangle
                    in
                    vertexOfTransformed |> Expect.point2d transformedVertex
                )
    in
    Test.describe "Vertices are consistent through transformation"
        [ testVertex "Bottom left" (\rectangle -> Rectangle2d.interpolate rectangle 0 0)
        , testVertex "Bottom right" (\rectangle -> Rectangle2d.interpolate rectangle 1 0)
        , testVertex "Top left" (\rectangle -> Rectangle2d.interpolate rectangle 1 1)
        , testVertex "Top right" (\rectangle -> Rectangle2d.interpolate rectangle 0 1)
        ]
