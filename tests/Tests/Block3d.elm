module Tests.Block3d exposing
    ( containmentIsConsistent
    , verticesAreConsistent
    )

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Block3d exposing (Block3d)
import Expect
import Frame3d
import Geometry.Expect as Expect
import Geometry.Random as Random
import Length exposing (Meters)
import LineSegment3d exposing (LineSegment3d)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Random exposing (Generator)
import Test exposing (Test)
import Test.Random as Test
import Vector3d exposing (Vector3d)


type alias Transformation coordinates =
    { block : Block3d Meters coordinates -> Block3d Meters coordinates
    , point : Point3d Meters coordinates -> Point3d Meters coordinates
    , lineSegment : LineSegment3d Meters coordinates -> LineSegment3d Meters coordinates
    , isZeroScale : Bool
    }


rotation : Axis3d Meters coordinates -> Angle -> Transformation coordinates
rotation axis angle =
    { block = Block3d.rotateAround axis angle
    , point = Point3d.rotateAround axis angle
    , lineSegment = LineSegment3d.rotateAround axis angle
    , isZeroScale = False
    }


translation : Vector3d Meters coordinates -> Transformation coordinates
translation displacement =
    { block = Block3d.translateBy displacement
    , point = Point3d.translateBy displacement
    , lineSegment = LineSegment3d.translateBy displacement
    , isZeroScale = False
    }


scaling : Point3d Meters coordinates -> Float -> Transformation coordinates
scaling centerPoint scale =
    { block = Block3d.scaleAbout centerPoint scale
    , point = Point3d.scaleAbout centerPoint scale
    , lineSegment = LineSegment3d.scaleAbout centerPoint scale
    , isZeroScale = scale == 0.0
    }


mirroring : Plane3d Meters coordinates -> Transformation coordinates
mirroring plane =
    { block = Block3d.mirrorAcross plane
    , point = Point3d.mirrorAcross plane
    , lineSegment = LineSegment3d.mirrorAcross plane
    , isZeroScale = False
    }


transformationGenerator : Generator (Transformation coordinates)
transformationGenerator =
    Random.oneOf
        (Random.map2 rotation Random.axis3d Random.angle)
        [ Random.map translation Random.vector3d
        , Random.map2 scaling Random.point3d Random.scale
        , Random.map mirroring Random.plane3d
        ]


containmentIsConsistent : Test
containmentIsConsistent =
    Test.check3 "Block/point containment is consistent through transformation"
        transformationGenerator
        Random.block3d
        Random.point3d
        (\transformation block point ->
            let
                initialContainment =
                    Block3d.contains point block

                transformedPoint =
                    transformation.point point

                transformedBlock =
                    transformation.block block

                finalContainment =
                    Block3d.contains transformedPoint transformedBlock
            in
            if transformation.isZeroScale then
                finalContainment |> Expect.equal True

            else
                finalContainment |> Expect.equal initialContainment
        )


verticesAreConsistent : Test
verticesAreConsistent =
    let
        testVertex description accessor =
            Test.check2 description
                transformationGenerator
                Random.block3d
                (\transformation block ->
                    let
                        vertex =
                            accessor block

                        transformedBlock =
                            transformation.block block

                        transformedVertex =
                            transformation.point vertex

                        vertexOfTransformed =
                            accessor transformedBlock
                    in
                    vertexOfTransformed |> Expect.point3d transformedVertex
                )
    in
    Test.describe "Vertices are consistent through transformation"
        [ testVertex "Back top left" (\block -> Block3d.interpolate block 0 1 1)
        , testVertex "Back bottom right" (\block -> Block3d.interpolate block 0 0 0)
        , testVertex "Front bottom left" (\block -> Block3d.interpolate block 1 1 0)
        , testVertex "Front top right" (\block -> Block3d.interpolate block 1 0 1)
        ]
