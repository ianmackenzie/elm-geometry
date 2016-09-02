{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Fuzz exposing (scalar)

import Fuzz exposing (Fuzzer)


scalar : Fuzzer Float
scalar =
    Fuzz.floatRange -10 10
