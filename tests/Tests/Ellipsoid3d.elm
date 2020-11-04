module Tests.Ellipsoid3d exposing
    ( equivalences
    , transformations
    )

import Axis3d exposing (Axis3d)
import Ellipsoid3d exposing (Ellipsoid3d)
import Expect
import Frame3d exposing (Frame3d)
import Fuzz
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Length exposing (Meters, meters)
import Point3d exposing (Point3d)
import Quantity
import Quantity.Interval as Interval exposing (Interval)
import Sphere3d exposing (Sphere3d)
import Test exposing (Test)



-- Helper methods


ellipsoidAtOriginWithRadii : ( Float, Float, Float ) -> Ellipsoid3d Meters coordinates
ellipsoidAtOriginWithRadii ( x, y, z ) =
    Ellipsoid3d.with
        { axes = Frame3d.atOrigin
        , xRadius = Length.meters x
        , yRadius = Length.meters y
        , zRadius = Length.meters z
        }



-- Tests


equivalences : Test
equivalences =
    Test.describe "Equivalent shapes"
        [ Test.fuzz Fuzz.float "Ellipsoid with 3 equal radii is equivalent to a sphere" <|
            \radius ->
                let
                    sphere =
                        Sphere3d.atOrigin (meters radius)

                    ellipsoid =
                        ellipsoidAtOriginWithRadii ( radius, radius, radius )
                in
                Ellipsoid3d.volume ellipsoid
                    |> Expect.equal (Sphere3d.volume sphere)
        ]


transformations : Test
transformations =
    Test.describe "Transforming two objects equally maintains their relative properties"
        [ Test.fuzz2
            (Fuzz.tuple ( Fuzz.ellipsoid3d, Fuzz.point3d ))
            Fuzz.vector3d
            "contains before/after translation"
            (\( ellipsoid, point ) vector ->
                let
                    beforeTransformation =
                        Ellipsoid3d.contains point ellipsoid

                    afterTransformation =
                        Ellipsoid3d.contains
                            (Point3d.translateBy vector point)
                            (Ellipsoid3d.translateBy vector ellipsoid)
                in
                Expect.equal beforeTransformation afterTransformation
            )
        , Test.fuzz2
            (Fuzz.tuple ( Fuzz.ellipsoid3d, Fuzz.point3d ))
            Fuzz.plane3d
            "contains before/after mirroring"
            (\( ellipsoid, point ) plane ->
                let
                    beforeTransformation =
                        Ellipsoid3d.contains point ellipsoid

                    afterTransformation =
                        Ellipsoid3d.contains
                            (Point3d.mirrorAcross plane point)
                            (Ellipsoid3d.mirrorAcross plane ellipsoid)
                in
                Expect.equal beforeTransformation afterTransformation
            )
        , Test.fuzz2
            (Fuzz.tuple ( Fuzz.ellipsoid3d, Fuzz.point3d ))
            (Fuzz.tuple ( Fuzz.axis3d, Fuzz.angle ))
            "contains before/after rotating"
            (\( ellipsoid, point ) ( axis, angle ) ->
                let
                    beforeTransformation =
                        Ellipsoid3d.contains point ellipsoid

                    afterTransformation =
                        Ellipsoid3d.contains
                            (Point3d.rotateAround axis angle point)
                            (Ellipsoid3d.rotateAround axis angle ellipsoid)
                in
                Expect.equal beforeTransformation afterTransformation
            )
        , Test.fuzz2
            (Fuzz.tuple ( Fuzz.ellipsoid3d, Fuzz.axis3d ))
            Fuzz.vector3d
            "signedDistanceAlong before/after translation"
            (\( ellipsoid, axis ) vector ->
                let
                    beforeTransformation =
                        Ellipsoid3d.signedDistanceAlong axis ellipsoid

                    afterTransformation =
                        Ellipsoid3d.signedDistanceAlong
                            (Axis3d.translateBy vector axis)
                            (Ellipsoid3d.translateBy vector ellipsoid)
                in
                afterTransformation
                    |> Expect.all
                        [ Interval.minValue >> Expect.quantity (Interval.minValue beforeTransformation)
                        , Interval.maxValue >> Expect.quantity (Interval.maxValue beforeTransformation)
                        ]
            )
        , Test.fuzz2
            (Fuzz.tuple ( Fuzz.ellipsoid3d, Fuzz.axis3d ))
            Fuzz.plane3d
            "signedDistanceAlong before/after mirroring"
            (\( ellipsoid, axis ) plane ->
                let
                    beforeTransformation =
                        Ellipsoid3d.signedDistanceAlong axis ellipsoid

                    afterTransformation =
                        Ellipsoid3d.signedDistanceAlong
                            (Axis3d.mirrorAcross plane axis)
                            (Ellipsoid3d.mirrorAcross plane ellipsoid)
                in
                afterTransformation
                    |> Expect.all
                        [ Interval.minValue >> Expect.quantity (Interval.minValue beforeTransformation)
                        , Interval.maxValue >> Expect.quantity (Interval.maxValue beforeTransformation)
                        ]
            )
        , Test.fuzz2
            (Fuzz.tuple ( Fuzz.ellipsoid3d, Fuzz.axis3d ))
            (Fuzz.tuple ( Fuzz.axis3d, Fuzz.angle ))
            "signedDistanceAlong before/after rotating"
            (\( ellipsoid, axis ) ( rotAxis, angle ) ->
                let
                    beforeTransformation =
                        Ellipsoid3d.signedDistanceAlong axis ellipsoid

                    afterTransformation =
                        Ellipsoid3d.signedDistanceAlong
                            (Axis3d.rotateAround rotAxis angle axis)
                            (Ellipsoid3d.rotateAround rotAxis angle ellipsoid)
                in
                afterTransformation
                    |> Expect.all
                        [ Interval.minValue >> Expect.quantity (Interval.minValue beforeTransformation)
                        , Interval.maxValue >> Expect.quantity (Interval.maxValue beforeTransformation)
                        ]
            )
        ]
