module Arc2d
    exposing
        ( Arc2d
        , throughPoints
        , fromEndpoints
        , sweptAround
        , centerPoint
        , radius
        , startPoint
        , endPoint
        )

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.WindingDirection as WindingDirection exposing (WindingDirection)


type Arc2d
    = Arc2d { frame : Frame2d, angle : Float }


notImplemented =
    Debug.crash "TODO"


throughPoints : Point2d -> Point2d -> Point2d -> Maybe Arc2d
throughPoints firstPoint secondPoint thirdPoint =
    notImplemented


fromEndpoints : Point2d -> Point2d -> WindingDirection -> Float -> Maybe Arc2d
fromEndpoints startPoint endPoint windingDirection radius =
    notImplemented


sweptAround : Point2d -> Point2d -> Float -> Arc2d
sweptAround centerPoint startPoint sweptAngle =
    notImplemented


centerPoint : Arc2d -> Point2d
centerPoint (Arc2d properties) =
    properties.centerPoint
