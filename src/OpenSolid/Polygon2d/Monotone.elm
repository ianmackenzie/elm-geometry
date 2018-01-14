module OpenSolid.Polygon2d.Monotone
    exposing
        ( polygons
        )

import Array.Hamt as Array exposing (Array)
import OpenSolid.Geometry.Internal as Internal exposing (Polygon2d(..))
import OpenSolid.Point2d as Point2d exposing (Point2d)


type alias Edge =
    { startVertex : Int
    , endVertex : Int
    , nextEdge : Int
    , previousEdge : Int
    }


type alias Loops =
    { vertices : Array Point2d
    , edges : Array Edge
    }


init : Polygon2d -> Loops
init (Polygon2d { outerLoop, innerLoops }) =
    let
        allLoops =
            outerLoop :: innerLoops

        vertices =
            Array.fromList (List.concat allLoops)
    in
    { vertices = vertices
    , edges = Array.empty
    }


polygons : Polygon2d -> ( Array Point2d, List (Array Int) )
polygons polygon2d =
    ( Array.empty, [] )
