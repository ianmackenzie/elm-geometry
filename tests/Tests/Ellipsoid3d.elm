module Tests.Ellipsoid3d exposing
    ( equivalences
    , transformations
    )

import Axis3d exposing (Axis3d)
import Ellipsoid3d exposing (Ellipsoid3d)
import Expect
import Frame3d exposing (Frame3d)
import Geometry.Expect as Expect
import Geometry.Random as Random
import Length exposing (Length, Meters, meters)
import Point3d exposing (Point3d)
import Quantity
import Quantity.Interval as Interval exposing (Interval)
import Sphere3d exposing (Sphere3d)
import Test exposing (Test)
import Test.Check as Test



-- Helper methods


ellipsoidAtOriginWithRadii : ( Length, Length, Length ) -> Ellipsoid3d Meters coordinates
ellipsoidAtOriginWithRadii ( x, y, z ) =
    Ellipsoid3d.with
        { axes = Frame3d.atOrigin
        , xRadius = x
        , yRadius = y
        , zRadius = z
        }



-- Tests


equivalences : Test
equivalences =
    Test.describe "Equivalent shapes"
        [ Test.check1 "Ellipsoid with 3 equal radii is equivalent to a sphere" Random.positiveLength <|
            \radius ->
                let
                    sphere =
                        Sphere3d.atOrigin radius

                    ellipsoid =
                        ellipsoidAtOriginWithRadii ( radius, radius, radius )
                in
                Ellipsoid3d.volume ellipsoid
                    |> Expect.equal (Sphere3d.volume sphere)
        ]


transformations : Test
transformations =
    Test.describe "Transforming two objects equally maintains their relative properties"
        [ Test.check3 "contains before/after translation"
            Random.ellipsoid3d
            Random.point3d
            Random.vector3d
            (\ellipsoid point vector ->
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
        , Test.check3 "contains before/after mirroring"
            Random.ellipsoid3d
            Random.point3d
            Random.plane3d
            (\ellipsoid point plane ->
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
        , Test.check4 "contains before/after rotating"
            Random.ellipsoid3d
            Random.point3d
            Random.axis3d
            Random.angle
            (\ellipsoid point axis angle ->
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
        , Test.check3 "signedDistanceAlong before/after translation"
            Random.ellipsoid3d
            Random.axis3d
            Random.vector3d
            (\ellipsoid axis vector ->
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
        , Test.check3 "signedDistanceAlong before/after mirroring"
            Random.ellipsoid3d
            Random.axis3d
            Random.plane3d
            (\ellipsoid axis plane ->
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
        , Test.check4 "signedDistanceAlong before/after rotating"
            Random.ellipsoid3d
            Random.axis3d
            Random.axis3d
            Random.angle
            (\ellipsoid axis rotAxis angle ->
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
