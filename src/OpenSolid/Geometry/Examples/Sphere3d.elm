module OpenSolid.Geometry.Examples.Sphere3d exposing (exampleSphere)

import OpenSolid.Point3d as Point3d
import OpenSolid.Sphere3d as Sphere3d exposing (Sphere3d)


exampleSphere : Sphere3d
exampleSphere =
    Sphere3d.with
        { centerPoint =
            Point3d.fromCoordinates ( 1, 2, 1 )
        , radius = 3
        }
