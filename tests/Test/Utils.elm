{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module Test.Utils
    exposing
        ( valueIsZero
        , valueIsOne
        , valuesAreEqual
        , vectorIsZero2d
        , vectorIsZero3d
        , vectorsAreEqual2d
        , vectorsAreEqual3d
        , directionsAreEqual2d
        , directionsAreEqual3d
        , pointsAreEqual2d
        , pointsAreEqual3d
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Direction2d as Direction2d
import OpenSolid.Core.Direction3d as Direction3d
import OpenSolid.Core.Point2d as Point2d
import OpenSolid.Core.Point3d as Point3d


valueIsZero : Float -> Bool
valueIsZero value =
    abs value < 1.0e-12


valueIsOne : Float -> Bool
valueIsOne value =
    valueIsZero (value - 1)


valuesAreEqual : Float -> Float -> Bool
valuesAreEqual first second =
    valueIsZero (first - second)


vectorIsZero2d : Vector2d -> Bool
vectorIsZero2d =
    Vector2d.length >> valueIsZero


vectorIsZero3d : Vector3d -> Bool
vectorIsZero3d =
    Vector3d.length >> valueIsZero


vectorsAreEqual2d : Vector2d -> Vector2d -> Bool
vectorsAreEqual2d firstVector secondVector =
    vectorIsZero2d (Vector2d.minus secondVector firstVector)


vectorsAreEqual3d : Vector3d -> Vector3d -> Bool
vectorsAreEqual3d firstVector secondVector =
    vectorIsZero3d (Vector3d.minus secondVector firstVector)


directionsAreEqual2d firstDirection secondDirection =
    vectorsAreEqual2d (Vector2d (Direction2d.components firstDirection))
        (Vector2d (Direction2d.components secondDirection))


directionsAreEqual3d firstDirection secondDirection =
    vectorsAreEqual3d (Vector3d (Direction3d.components firstDirection))
        (Vector3d (Direction3d.components secondDirection))


pointsAreEqual2d firstPoint secondPoint =
    vectorIsZero2d (Point2d.vectorFrom firstPoint secondPoint)


pointsAreEqual3d firstPoint secondPoint =
    vectorIsZero3d (Point3d.vectorFrom firstPoint secondPoint)
