module Geometry.Examples.Sphere3d exposing (exampleSphere)

import Point3d
import Sphere3d exposing (Sphere3d)


exampleSphere : Sphere3d
exampleSphere =
    Sphere3d.with
        { centerPoint =
            Point3d.fromCoordinates ( 1, 2, 1 )
        , radius = 3
        }
