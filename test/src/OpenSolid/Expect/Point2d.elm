module OpenSolid.Expect.Point2d exposing (point2d, point2dWithin)

import Expect exposing (Expectation)
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Expect as Expect
import OpenSolid.Compare.Point2d as Compare


point2d : Point2d -> Point2d -> Expectation
point2d =
    Expect.by Compare.point2d


point2dWithin : Float -> Point2d -> Point2d -> Expectation
point2dWithin =
    Expect.by << Compare.point2dWithin
