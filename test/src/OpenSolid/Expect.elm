{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Expect
    exposing
        ( by
        , approximately
        , approximatelyWithin
        , angle
        , angleWithin
        )

import Expect exposing (Expectation)
import OpenSolid.Compare as Compare exposing (Comparator)


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
