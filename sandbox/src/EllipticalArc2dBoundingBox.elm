module EllipticalArc2dBoundingBox exposing (main)

import BoundingBox2dTest
import Drawing2d
import EllipticalArc2d
import Random2d


main : BoundingBox2dTest.Program
main =
    BoundingBox2dTest.program
        Random2d.ellipticalArc
        EllipticalArc2d.boundingBox
        (Drawing2d.ellipticalArc [])
