module EllipticalArc2dDistanceAlongAxis exposing (main)

import DistanceAlongAxis2dTest
import Drawing2d
import EllipticalArc2d
import Random2d


main : DistanceAlongAxis2dTest.Program
main =
    DistanceAlongAxis2dTest.program
        Random2d.ellipticalArc
        EllipticalArc2d.signedDistanceAlong
        (Drawing2d.ellipticalArc [])
