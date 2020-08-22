module Arc2dBoundingBox exposing (main)

import Angle
import Arc2d exposing (Arc2d)
import BoundingBox2d exposing (BoundingBox2d)
import BoundingBox2dTest
import Drawing2d
import LineSegment2d
import Random exposing (Generator)
import Random2d


arcGenerator : BoundingBox2d units coordinates -> Generator (Arc2d units coordinates)
arcGenerator bounds =
    Random.weighted
        ( 0.8, Random2d.circularArc bounds )
        [ ( 0.2
          , Random2d.lineSegment bounds
                |> Random.map
                    (\lineSegment ->
                        Arc2d.from
                            (LineSegment2d.startPoint lineSegment)
                            (LineSegment2d.endPoint lineSegment)
                            (Angle.degrees 0)
                    )
          )
        ]
        |> Random.andThen identity


main : BoundingBox2dTest.Program
main =
    BoundingBox2dTest.program
        arcGenerator
        Arc2d.boundingBox
        (Drawing2d.arc [])
