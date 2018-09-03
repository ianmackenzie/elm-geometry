--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module FillsAndStrokes exposing
    ( blackStroke
    , dashed
    , greyFill
    , greyStroke
    , noFill
    , whiteFill
    )

import Svg
import Svg.Attributes as Attributes


whiteFill : Svg.Attribute msg
whiteFill =
    Attributes.fill "white"


greyFill : Svg.Attribute msg
greyFill =
    Attributes.fill "grey"


noFill : Svg.Attribute msg
noFill =
    Attributes.fill "none"


blackStroke : Svg.Attribute msg
blackStroke =
    Attributes.stroke "black"


greyStroke : Svg.Attribute msg
greyStroke =
    Attributes.stroke "grey"


dashed : Svg.Attribute msg
dashed =
    Attributes.strokeDasharray "5 5"
