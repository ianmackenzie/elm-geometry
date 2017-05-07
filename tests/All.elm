--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--                                                                            --
-- Copyright 2016 by Ian Mackenzie                                            --
-- ian.e.mackenzie@gmail.com                                                  --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module All exposing (..)

import Test
import Test.Runner.Html as HtmlRunner
import Arc2d
import Arc3d
import Axis2d
import Axis3d
import BoundingBox2d
import BoundingBox3d
import Circle2d
import Circle3d
import CubicSpline2d
import CubicSpline3d
import Direction2d
import Direction3d
import Frame2d
import Frame3d
import LineSegment2d
import LineSegment3d
import Plane3d
import Point2d
import Point3d
import Polygon2d
import Polyline2d
import Polyline3d
import QuadraticSpline2d
import QuadraticSpline3d
import Triangle2d
import Triangle3d
import Vector2d
import Vector3d


suite =
    Test.describe "OpenSolid.Geometry"
        [ Arc2d.suite
        , Arc3d.suite
        , Axis2d.suite
        , Axis3d.suite
        , BoundingBox2d.suite
        , BoundingBox3d.suite
        , Circle2d.suite
        , Circle3d.suite
        , CubicSpline2d.suite
        , CubicSpline3d.suite
        , Direction2d.suite
        , Direction3d.suite
        , Frame2d.suite
        , Frame3d.suite
        , LineSegment2d.suite
        , LineSegment3d.suite
        , Plane3d.suite
        , Point2d.suite
        , Point3d.suite
        , Polygon2d.suite
        , Polyline2d.suite
        , Polyline3d.suite
        , QuadraticSpline2d.suite
        , QuadraticSpline3d.suite
        , Triangle2d.suite
        , Triangle3d.suite
        , Vector2d.suite
        , Vector3d.suite
        ]


main : HtmlRunner.TestProgram
main =
    HtmlRunner.run suite
