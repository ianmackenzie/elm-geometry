module FillsAndStrokes exposing (..)

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
