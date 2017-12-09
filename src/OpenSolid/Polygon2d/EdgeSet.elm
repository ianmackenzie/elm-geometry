module OpenSolid.Polygon2d.EdgeSet
    exposing
        ( EdgeSet
        , insert
        , remove
        )

import OpenSolid.LineSegment2d exposing (LineSegment2d)


type EdgeSet
    = Node Int LineSegment2d EdgeSet EdgeSet
    | Empty


singleton : LineSegment2d -> EdgeSet
singleton lineSegment =
    Node 1 lineSegment empty empty


empty : EdgeSet
empty =
    Empty





insert : LineSegment2d -> EdgeSet -> EdgeSet
insert lineSegment edgeSet =
    case edgeSet of
        Empty ->
            singleton lineSegment

        Node height head left right ->

