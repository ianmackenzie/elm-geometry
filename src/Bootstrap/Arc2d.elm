module Bootstrap.Arc2d exposing (sweptAround)

import Geometry.Internal exposing (..)


sweptAround : Point2d -> Float -> Point2d -> Arc2d
sweptAround centerPoint sweptAngle startPoint =
    Arc2d
        { centerPoint = centerPoint
        , sweptAngle = sweptAngle
        , startPoint = startPoint
        }
