{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.Test.Expect
    exposing
        ( Expector
        , approximately
        , approximatelyWithin
        , vector2d
        , vector2dWithin
        , vector3d
        , vector3dWithin
        , direction2d
        , direction2dWithin
        , direction3d
        , direction3dWithin
        , point2d
        , point2dWithin
        , point3d
        , point3dWithin
        , axis2d
        , axis2dWithin
        , axis3d
        , axis3dWithin
        )

import Expect exposing (Expectation)
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Direction2d as Direction2d
import OpenSolid.Core.Direction3d as Direction3d
import OpenSolid.Core.Point2d as Point2d
import OpenSolid.Core.Point3d as Point3d
import OpenSolid.Core.Test.Compare as Compare exposing (Comparator)


type alias Expector a =
    a -> a -> Expectation


by : Comparator a -> Expector a
by comparator first second =
    if comparator first second then
        Expect.pass
    else
        let
            message =
                "Expected " ++ toString first ++ ", got " ++ toString second
        in
            Expect.fail message


approximately : Expector Float
approximately =
    by Compare.approximately


approximatelyWithin : Float -> Expector Float
approximatelyWithin =
    by << Compare.approximatelyWithin


vector2d : Vector2d -> Vector2d -> Expectation
vector2d =
    by Compare.vector2d


vector2dWithin : Float -> Vector2d -> Vector2d -> Expectation
vector2dWithin =
    by << Compare.vector2dWithin


vector3d : Vector3d -> Vector3d -> Expectation
vector3d =
    by Compare.vector3d


vector3dWithin : Float -> Vector3d -> Vector3d -> Expectation
vector3dWithin =
    by << Compare.vector3dWithin


direction2d : Direction2d -> Direction2d -> Expectation
direction2d =
    by Compare.direction2d


direction2dWithin : Float -> Direction2d -> Direction2d -> Expectation
direction2dWithin =
    by << Compare.direction2dWithin


direction3d : Direction3d -> Direction3d -> Expectation
direction3d =
    by Compare.direction3d


direction3dWithin : Float -> Direction3d -> Direction3d -> Expectation
direction3dWithin =
    by << Compare.direction3dWithin


point2d : Point2d -> Point2d -> Expectation
point2d =
    by Compare.point2d


point2dWithin : Float -> Point2d -> Point2d -> Expectation
point2dWithin =
    by << Compare.point2dWithin


point3d : Point3d -> Point3d -> Expectation
point3d =
    by Compare.point3d


point3dWithin : Float -> Point3d -> Point3d -> Expectation
point3dWithin =
    by << Compare.point3dWithin


axis2d : Axis2d -> Axis2d -> Expectation
axis2d =
    by Compare.axis2d


axis2dWithin : Float -> Axis2d -> Axis2d -> Expectation
axis2dWithin =
    by << Compare.axis2dWithin


axis3d : Axis3d -> Axis3d -> Expectation
axis3d =
    by Compare.axis3d


axis3dWithin : Float -> Axis3d -> Axis3d -> Expectation
axis3dWithin =
    by << Compare.axis3dWithin
