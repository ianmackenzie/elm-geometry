module OpenSolid.Internal.CubicSpline3d exposing (derivativeMagnitudeBounds)

import OpenSolid.Geometry.Internal as Internal
import OpenSolid.Vector3d as Vector3d exposing (Vector3d)


lineMin : Vector3d -> Vector3d -> Vector3d -> Float -> Float -> Float -> Float -> Float
lineMin vA vB vAB aSquared bSquared a b =
    let
        abSquared =
            Vector3d.squaredLength vAB
    in
    if abSquared == 0 then
        a
    else if aSquared >= bSquared + abSquared then
        b
    else if bSquared >= aSquared + abSquared then
        a
    else
        sqrt (Vector3d.squaredLength (Vector3d.crossProduct vA vB) / abSquared)


derivativeMagnitudeBounds : Internal.CubicSpline3d -> Internal.Interval
derivativeMagnitudeBounds (Internal.CubicSpline3d ( p1, p2, p3, p4 )) =
    let
        vA =
            Vector3d.from p1 p2

        vB =
            Vector3d.from p2 p3

        vC =
            Vector3d.from p3 p4

        aSquared =
            Vector3d.squaredLength vA

        bSquared =
            Vector3d.squaredLength vB

        cSquared =
            Vector3d.squaredLength vC

        a =
            sqrt aSquared

        b =
            sqrt bSquared

        c =
            sqrt cSquared

        vAB =
            Vector3d.difference vB vA

        vBC =
            Vector3d.difference vC vB

        vCA =
            Vector3d.difference vA vC

        vN =
            Vector3d.crossProduct vAB vBC

        aAB =
            Vector3d.dotProduct (Vector3d.crossProduct vA vB) vN

        aBC =
            Vector3d.dotProduct (Vector3d.crossProduct vB vC) vN

        aCA =
            Vector3d.dotProduct (Vector3d.crossProduct vC vA) vN

        minValue =
            if aAB > 0 && aBC > 0 && aCA > 0 then
                3 * abs (Vector3d.dotProduct vA vN / Vector3d.length vN)
            else
                let
                    minAB =
                        lineMin vA vB vAB aSquared bSquared a b

                    minBC =
                        lineMin vB vC vBC bSquared cSquared b c

                    minCA =
                        lineMin vC vA vCA cSquared aSquared c a
                in
                3 * min minAB (min minBC minCA)

        maxValue =
            3 * max a (max b c)
    in
    Internal.Interval { minValue = minValue, maxValue = maxValue }
