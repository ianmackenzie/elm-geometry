--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Bootstrap.Direction3d exposing
    ( components
    , unsafe
    )

import Geometry.Types exposing (..)


unsafe : ( Float, Float, Float ) -> Direction3d coordinates
unsafe givenComponents =
    Direction3d givenComponents


components : Direction3d coordinates -> ( Float, Float, Float )
components (Direction3d givenComponents) =
    givenComponents
