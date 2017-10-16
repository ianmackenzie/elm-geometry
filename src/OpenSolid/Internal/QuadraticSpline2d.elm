module OpenSolid.Internal.QuadraticSpline2d exposing (derivativeMagnitudeBounds)

import OpenSolid.Geometry.Internal as Internal
import OpenSolid.Vector2d as Vector2d


derivativeMagnitudeBounds : Internal.QuadraticSpline2d -> Internal.Interval
derivativeMagnitudeBounds (Internal.QuadraticSpline2d ( p1, p2, p3 )) =
    let
        vA =
            Vector2d.from p1 p2

        vB =
            Vector2d.from p2 p3

        vAB =
            Vector2d.difference vB vA

        aSquared =
            Vector2d.squaredLength vA

        bSquared =
            Vector2d.squaredLength vB

        abSquared =
            Vector2d.squaredLength vAB

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
            { minValue = 2 * abs (Vector2d.crossProduct vA vB) / sqrt abSquared
            , maxValue = 2 * max a b
            }
