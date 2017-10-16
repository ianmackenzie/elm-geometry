module OpenSolid.Internal.CubicSpline2d exposing (derivativeMagnitudeBounds)

import OpenSolid.Geometry.Internal as Internal
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)


lineMin : Vector2d -> Vector2d -> Vector2d -> Float -> Float -> Float -> Float -> Float
lineMin vA vB vAB aSquared bSquared a b =
    let
        abSquared =
            Vector2d.squaredLength vAB
    in
    if abSquared == 0 then
        a
    else if aSquared >= bSquared + abSquared then
        b
    else if bSquared >= aSquared + abSquared then
        a
    else
        abs (Vector2d.crossProduct vA vB) / sqrt abSquared


derivativeMagnitudeBounds : Internal.CubicSpline2d -> Internal.Interval
derivativeMagnitudeBounds (Internal.CubicSpline2d ( p1, p2, p3, p4 )) =
    let
        vA =
            Vector2d.from p1 p2

        vB =
            Vector2d.from p2 p3

        vC =
            Vector2d.from p3 p4

        vAB =
            Vector2d.difference vB vA

        vBC =
            Vector2d.difference vC vB

        vCA =
            Vector2d.difference vA vC

        aSquared =
            Vector2d.squaredLength vA

        bSquared =
            Vector2d.squaredLength vB

        cSquared =
            Vector2d.squaredLength vC

        a =
            sqrt aSquared

        b =
            sqrt bSquared

        c =
            sqrt cSquared

        maxValue =
            3 * max a (max b c)

        aArea =
            Vector2d.crossProduct vB vBC

        bArea =
            Vector2d.crossProduct vC vCA

        cArea =
            Vector2d.crossProduct vA vAB

        minValue =
            if aArea >= 0 && bArea >= 0 && cArea >= 0 then
                0
            else if aArea <= 0 && bArea <= 0 && cArea <= 0 then
                0
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
    in
    Internal.Interval { minValue = minValue, maxValue = maxValue }
