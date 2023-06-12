module Tests.Frame2d exposing (globalToGlobal, localToLocal)

import Frame2d
import Geometry.Expect as Expect
import Geometry.Random as Random
import Point2d
import Test exposing (Test)
import Test.Random as Test


globalToGlobal : Test
globalToGlobal =
    let
        description =
            "Global -> local -> global conversion round-trips properly"

        expectation frame point =
            point
                |> Point2d.relativeTo frame
                |> Point2d.placeIn frame
                |> Expect.point2d point
    in
    Test.check2 description Random.frame2d Random.point2d expectation


localToLocal : Test
localToLocal =
    let
        description =
            "Local -> global -> local conversion round-trips properly"

        expectation frame point =
            point
                |> Point2d.placeIn frame
                |> Point2d.relativeTo frame
                |> Expect.point2d point
    in
    Test.check2 description Random.frame2d Random.point2d expectation
