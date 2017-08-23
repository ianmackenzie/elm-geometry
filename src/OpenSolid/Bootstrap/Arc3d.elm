module OpenSolid.Bootstrap.Arc3d exposing (around)

import OpenSolid.Geometry.Internal exposing (..)


around : Axis3d -> { startPoint : Point3d, sweptAngle : Float } -> Arc3d
around axis { startPoint, sweptAngle } =
    Arc3d { axis = axis, startPoint = startPoint, sweptAngle = sweptAngle }
