{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.Expect
    exposing
        ( by
        , approximately
        , approximatelyWithin
        , angle
        , angleWithin
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
        , plane3d
        , plane3dWithin
        , frame2d
        , frame2dWithin
        , frame3d
        , frame3dWithin
        , sketchPlane3d
        , sketchPlane3dWithin
        , boundingBox2d
        , boundingBox2dWithin
        , boundingBox3d
        , boundingBox3dWithin
        )

import Expect exposing (Expectation)
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Compare as Compare exposing (Comparator)


by : Comparator a -> (a -> a -> Expectation)
by comparator first second =
    if comparator first second then
        Expect.pass
    else
        let
            message =
                "Expected " ++ toString first ++ ", got " ++ toString second
        in
            Expect.fail message


approximately : Float -> Float -> Expectation
approximately =
    by Compare.approximately


approximatelyWithin : Float -> Float -> Float -> Expectation
approximatelyWithin =
    by << Compare.approximatelyWithin


angle : Float -> Float -> Expectation
angle =
    by Compare.angle


angleWithin : Float -> Float -> Float -> Expectation
angleWithin =
    by << Compare.angleWithin


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


plane3d : Plane3d -> Plane3d -> Expectation
plane3d =
    by Compare.plane3d


plane3dWithin : Float -> Plane3d -> Plane3d -> Expectation
plane3dWithin =
    by << Compare.plane3dWithin


frame2d : Frame2d -> Frame2d -> Expectation
frame2d =
    by Compare.frame2d


frame2dWithin : Float -> Frame2d -> Frame2d -> Expectation
frame2dWithin =
    by << Compare.frame2dWithin


frame3d : Frame3d -> Frame3d -> Expectation
frame3d =
    by Compare.frame3d


frame3dWithin : Float -> Frame3d -> Frame3d -> Expectation
frame3dWithin =
    by << Compare.frame3dWithin


sketchPlane3d : SketchPlane3d -> SketchPlane3d -> Expectation
sketchPlane3d =
    by Compare.sketchPlane3d


sketchPlane3dWithin : Float -> SketchPlane3d -> SketchPlane3d -> Expectation
sketchPlane3dWithin =
    by << Compare.sketchPlane3dWithin


boundingBox2d : BoundingBox2d -> BoundingBox2d -> Expectation
boundingBox2d =
    by Compare.boundingBox2d


boundingBox2dWithin : Float -> BoundingBox2d -> BoundingBox2d -> Expectation
boundingBox2dWithin =
    by << Compare.boundingBox2dWithin


boundingBox3d : BoundingBox3d -> BoundingBox3d -> Expectation
boundingBox3d =
    by Compare.boundingBox3d


boundingBox3dWithin : Float -> BoundingBox3d -> BoundingBox3d -> Expectation
boundingBox3dWithin =
    by << Compare.boundingBox3dWithin
