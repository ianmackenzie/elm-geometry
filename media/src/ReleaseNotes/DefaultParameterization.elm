--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module ReleaseNotes.DefaultParameterization exposing (main)

import CubicSpline2d
import Drawing2d
import Geometry.Parameter as Parameter
import Html exposing (Html)
import ReleaseNotes.Common exposing (..)


main : Html Never
main =
    let
        points =
            CubicSpline2d.pointsOn spline (Parameter.numSteps numSegments)
    in
    Drawing2d.toHtml renderBounds
        []
        [ Drawing2d.cubicSpline spline
        , Drawing2d.dots points
        ]
