module OpenSolid.Bootstrap.Arc2d exposing (with)

import OpenSolid.Geometry.Internal exposing (..)


with : { centerPoint : Point2d, startPoint : Point2d, sweptAngle : Float } -> Arc2d
with =
    Arc2d
