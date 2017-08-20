module OpenSolid.Bootstrap.Arc3d exposing (with)

import OpenSolid.Geometry.Types exposing (..)


with : { axis : Axis3d, startPoint : Point3d, sweptAngle : Float } -> Arc3d
with =
    Arc3d
