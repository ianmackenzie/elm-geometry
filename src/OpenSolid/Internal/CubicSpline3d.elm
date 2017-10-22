module OpenSolid.Internal.CubicSpline3d exposing (derivativeMagnitudeBounds)

import OpenSolid.Geometry.Internal as Internal
import OpenSolid.Point3d as Point3d exposing (Point3d)
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


accurateBounds : Internal.CubicSpline3d -> Internal.Interval
accurateBounds (Internal.CubicSpline3d ( p1, p2, p3, p4 )) =
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


fastBounds : Internal.CubicSpline3d -> Internal.Interval
fastBounds (Internal.CubicSpline3d ( p1, p2, p3, p4 )) =
    let
        ( x1, y1, z1 ) =
            Point3d.coordinates p1

        ( x2, y2, z2 ) =
            Point3d.coordinates p2

        ( x3, y3, z3 ) =
            Point3d.coordinates p3

        ( x4, y4, z4 ) =
            Point3d.coordinates p4

        ax =
            x2 - x1

        ay =
            y2 - y1

        az =
            z2 - z1

        bx =
            x3 - x2

        by =
            y3 - y2

        bz =
            z3 - z2

        cx =
            x4 - x3

        cy =
            y4 - y3

        cz =
            z4 - z3

        xMin =
            min ax (min bx cx)

        yMin =
            min ay (min by cy)

        zMin =
            min az (min bz cz)

        xMax =
            max ax (max bx cx)

        yMax =
            max ay (max by cy)

        zMax =
            max az (max bz cz)

        xMinSquared =
            xMin * xMin

        yMinSquared =
            yMin * yMin

        zMinSquared =
            zMin * zMin

        xMaxSquared =
            xMax * xMax

        yMaxSquared =
            yMax * yMax

        zMaxSquared =
            zMax * zMax

        ( xSquaredMin, xSquaredMax ) =
            if xMin >= 0 then
                ( xMinSquared, xMaxSquared )
            else if xMax <= 0 then
                ( xMaxSquared, xMinSquared )
            else
                ( 0, max xMinSquared xMaxSquared )

        ( ySquaredMin, ySquaredMax ) =
            if yMin >= 0 then
                ( yMinSquared, yMaxSquared )
            else if yMax <= 0 then
                ( yMaxSquared, yMinSquared )
            else
                ( 0, max yMinSquared yMaxSquared )

        ( zSquaredMin, zSquaredMax ) =
            if zMin >= 0 then
                ( zMinSquared, zMaxSquared )
            else if zMax <= 0 then
                ( zMaxSquared, zMinSquared )
            else
                ( 0, max zMinSquared zMaxSquared )
    in
    Internal.Interval
        { minValue = 3 * sqrt (xSquaredMin + ySquaredMin + zSquaredMin)
        , maxValue = 3 * sqrt (xSquaredMax + ySquaredMax + zSquaredMax)
        }


derivativeMagnitudeBounds : Internal.CubicSpline3d -> Internal.Interval
derivativeMagnitudeBounds =
    accurateBounds
