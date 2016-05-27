{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module Test.Utils exposing (isApproximatelyZero, areApproximatelyEqual)


isApproximatelyZero : Float -> Bool
isApproximatelyZero value =
    abs value < 1.0e-12


areApproximatelyEqual : Float -> Float -> Bool
areApproximatelyEqual first second =
    isApproximatelyZero (first - second)
