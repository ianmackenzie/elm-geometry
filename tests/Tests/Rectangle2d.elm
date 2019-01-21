module Tests.Rectangle2d exposing
    ( containmentIsConsistent
    , edgesAreConsistent
    , verticesAreConsistent
    )

import Angle exposing (Angle)
import Axis2d
import Expect
import Frame2d
import Fuzz exposing (Fuzzer)
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Geometry.Test exposing (..)
import LineSegment2d
import Point2d
import Rectangle2d
import Test exposing (Test)
import Vector2d


type alias Transformation coordinates =
    { rectangle : Rectangle2d coordinates -> Rectangle2d coordinates
    , point : Point2d coordinates -> Point2d coordinates
    , lineSegment : LineSegment2d coordinates -> LineSegment2d coordinates
    }


rotation : Point2d coordinates -> Angle -> Transformation coordinates
rotation centerPoint angle =
    { rectangle = Rectangle2d.rotateAround centerPoint angle
    , point = Point2d.rotateAround centerPoint angle
    , lineSegment = LineSegment2d.rotateAround centerPoint angle
    }


translation : Vector2d coordinates -> Transformation coordinates
translation displacement =
    { rectangle = Rectangle2d.translateBy displacement
    , point = Point2d.translateBy displacement
    , lineSegment = LineSegment2d.translateBy displacement
    }


scaling : Point2d coordinates -> Float -> Transformation coordinates
scaling centerPoint scale =
    { rectangle = Rectangle2d.scaleAbout centerPoint scale
    , point = Point2d.scaleAbout centerPoint scale
    , lineSegment = LineSegment2d.scaleAbout centerPoint scale
    }


mirroring : Axis2d coordinates -> Transformation coordinates
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
        [ testVertex "Bottom left" Rectangle2d.bottomLeftVertex
        , testVertex "Bottom right" Rectangle2d.bottomRightVertex
        , testVertex "Top left" Rectangle2d.topLeftVertex
        , testVertex "Top right" Rectangle2d.topRightVertex
        ]


edgesAreConsistent : Test
edgesAreConsistent =
    let
        testEdge description accessor =
            Test.fuzz2
                transformationFuzzer
                Fuzz.rectangle2d
                description
                (\transformation rectangle ->
                    let
                        edge =
                            accessor rectangle

                        transformedRectangle =
                            transformation.rectangle rectangle

                        transformedEdge =
                            transformation.lineSegment edge

                        edgeOfTransformed =
                            accessor transformedRectangle
                    in
                    edgeOfTransformed |> Expect.lineSegment2d transformedEdge
                )
    in
    Test.describe "Edges are consistent through transformation"
        [ testEdge "Bottom" Rectangle2d.bottomEdge
        , testEdge "Top" Rectangle2d.topEdge
        , testEdge "Left" Rectangle2d.leftEdge
        , testEdge "Right" Rectangle2d.rightEdge
        ]
