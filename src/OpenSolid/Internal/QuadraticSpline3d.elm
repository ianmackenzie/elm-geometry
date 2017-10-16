module OpenSolid.Internal.QuadraticSpline3d exposing (derivativeMagnitudeBounds)

import OpenSolid.Geometry.Internal as Internal
import OpenSolid.Vector3d as Vector3d


derivativeMagnitudeBounds : Internal.QuadraticSpline3d -> Internal.Interval
derivativeMagnitudeBounds (Internal.QuadraticSpline3d ( p1, p2, p3 )) =
    let
        vA =
            Vector3d.from p1 p2

        vB =
            Vector3d.from p2 p3

        vAB =
            Vector3d.difference vB vA

        aSquared =
            Vector3d.squaredLength vA

        bSquared =
            Vector3d.squaredLength vB

        abSquared =
            Vector3d.squaredLength vAB

        a =
            sqrt aSquared

        b =
            sqrt bSquared
    in
    Internal.Interval <|
        if abSquared == 0 then
            { minValue = 2 * a, maxValue = 2 * a }
        else if aSquared >= bSquared + abSquared then
            { minValue = 2 * b, maxValue = 2 * a }
        else if bSquared >= aSquared + abSquared then
            { minValue = 2 * a, maxValue = 2 * b }
        else
            let
                squaredCrossProduct =
                    Vector3d.squaredLength (Vector3d.crossProduct vA vB)
            in
            { minValue = 2 * sqrt (squaredCrossProduct / abSquared)
            , maxValue = 2 * max a b
            }
