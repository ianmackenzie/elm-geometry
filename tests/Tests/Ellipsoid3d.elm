module Tests.Ellipsoid3d exposing (equivalences)

import Ellipsoid3d exposing (Ellipsoid3d)
import Expect
import Frame3d exposing (Frame3d)
import Fuzz
import Geometry.Expect as Expect
import Geometry.Fuzz as Fuzz
import Length exposing (Meters, meters)
import Point3d exposing (Point3d)
import Sphere3d exposing (Sphere3d)
import Test exposing (Test)



-- Helper methods


ellipsoidAtOriginWithRadii : ( Float, Float, Float ) -> Ellipsoid3d Meters coordinates
ellipsoidAtOriginWithRadii ( x, y, z ) =
    Ellipsoid3d.with
        { axes = Frame3d.atPoint (Point3d.meters 0 0 0)
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
                        Sphere3d.withRadius (meters radius) (Point3d.meters 0 0 0)

                    ellipsoid =
                        ellipsoidAtOriginWithRadii ( radius, radius, radius )
                in
                Ellipsoid3d.volume ellipsoid
                    |> Expect.equal (Sphere3d.volume sphere)
        ]
