--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module CubicSpline1d exposing
    ( CubicSpline1d
    , at
    , at_
    , boundingBox
    , endDerivative
    , endPoint
    , firstControlPoint
    , firstDerivative
    , firstDerivativeBoundingBox
    , fourthControlPoint
    , fromControlPoints
    , pointOn
    , secondControlPoint
    , secondDerivative
    , secondDerivativeBoundingBox
    , startDerivative
    , startPoint
    , thirdControlPoint
    , thirdDerivative
    )

import Geometry.Types as Types
import Quantity exposing (Quantity(..), Rate)
import Quantity.Interval as Interval exposing (Interval)


type alias CubicSpline1d units =
    Types.CubicSpline1d units


fromControlPoints : Quantity Float units -> Quantity Float units -> Quantity Float units -> Quantity Float units -> CubicSpline1d units
fromControlPoints p1 p2 p3 p4 =
    Types.CubicSpline1d
        { firstControlPoint = p1
        , secondControlPoint = p2
        , thirdControlPoint = p3
        , fourthControlPoint = p4
        }


at : Quantity Float (Rate units2 units1) -> CubicSpline1d units1 -> CubicSpline1d units2
at rate (Types.CubicSpline1d spline) =
    Types.CubicSpline1d
        { firstControlPoint = Quantity.at rate spline.firstControlPoint
        , secondControlPoint = Quantity.at rate spline.secondControlPoint
        , thirdControlPoint = Quantity.at rate spline.thirdControlPoint
        , fourthControlPoint = Quantity.at rate spline.fourthControlPoint
        }


at_ : Quantity Float (Rate units1 units2) -> CubicSpline1d units1 -> CubicSpline1d units2
at_ rate spline =
    at (Quantity.inverse rate) spline


startPoint : CubicSpline1d units -> Quantity Float units
startPoint (Types.CubicSpline1d spline) =
    spline.firstControlPoint


endPoint : CubicSpline1d units -> Quantity Float units
endPoint (Types.CubicSpline1d spline) =
    spline.fourthControlPoint


firstControlPoint : CubicSpline1d units -> Quantity Float units
firstControlPoint (Types.CubicSpline1d spline) =
    spline.firstControlPoint


secondControlPoint : CubicSpline1d units -> Quantity Float units
secondControlPoint (Types.CubicSpline1d spline) =
    spline.secondControlPoint


thirdControlPoint : CubicSpline1d units -> Quantity Float units
thirdControlPoint (Types.CubicSpline1d spline) =
    spline.thirdControlPoint


fourthControlPoint : CubicSpline1d units -> Quantity Float units
fourthControlPoint (Types.CubicSpline1d spline) =
    spline.fourthControlPoint


startDerivative : CubicSpline1d units -> Quantity Float units
startDerivative spline =
    Quantity.multiplyBy 3 (secondControlPoint spline |> Quantity.minus (firstControlPoint spline))


endDerivative : CubicSpline1d units -> Quantity Float units
endDerivative spline =
    Quantity.multiplyBy 3 (fourthControlPoint spline |> Quantity.minus (thirdControlPoint spline))


boundingBox : CubicSpline1d units -> Interval Float units
boundingBox spline =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        p4 =
            fourthControlPoint spline
    in
    Interval.union (Interval.from p1 p2) (Interval.from p3 p4)


pointOn : CubicSpline1d units -> Float -> Quantity Float units
pointOn spline parameterValue =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        p4 =
            fourthControlPoint spline

        q1 =
            Quantity.interpolateFrom p1 p2 parameterValue

        q2 =
            Quantity.interpolateFrom p2 p3 parameterValue

        q3 =
            Quantity.interpolateFrom p3 p4 parameterValue

        r1 =
            Quantity.interpolateFrom q1 q2 parameterValue

        r2 =
            Quantity.interpolateFrom q2 q3 parameterValue
    in
    Quantity.interpolateFrom r1 r2 parameterValue


firstDerivative : CubicSpline1d units -> Float -> Quantity Float units
firstDerivative spline parameterValue =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        p4 =
            fourthControlPoint spline

        v1 =
            p2 |> Quantity.minus p1

        v2 =
            p3 |> Quantity.minus p2

        v3 =
            p4 |> Quantity.minus p3

        w1 =
            Quantity.interpolateFrom v1 v2 parameterValue

        w2 =
            Quantity.interpolateFrom v2 v3 parameterValue
    in
    Quantity.multiplyBy 3 (Quantity.interpolateFrom w1 w2 parameterValue)


{-| Evaluate the second derivative of a spline at a given parameter value.
-}
secondDerivative : CubicSpline1d units -> Float -> Quantity Float units
secondDerivative spline parameterValue =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        p4 =
            fourthControlPoint spline

        u1 =
            p2 |> Quantity.minus p1

        u2 =
            p3 |> Quantity.minus p2

        u3 =
            p4 |> Quantity.minus p3

        v1 =
            u2 |> Quantity.minus u1

        v2 =
            u3 |> Quantity.minus u2
    in
    Quantity.multiplyBy 6 (Quantity.interpolateFrom v1 v2 parameterValue)


thirdDerivative : CubicSpline1d units -> Quantity Float units
thirdDerivative spline =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        p4 =
            fourthControlPoint spline

        u1 =
            p2 |> Quantity.minus p1

        u2 =
            p3 |> Quantity.minus p2

        u3 =
            p4 |> Quantity.minus p3

        v1 =
            u2 |> Quantity.minus u1

        v2 =
            u3 |> Quantity.minus u2
    in
    Quantity.multiplyBy 6 (v2 |> Quantity.minus v1)


{-| Get the bounds on the first derivative of a spline.
-}
firstDerivativeBoundingBox : CubicSpline1d units -> Interval Float units
firstDerivativeBoundingBox spline =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        p4 =
            fourthControlPoint spline

        u1 =
            Quantity.multiplyBy 3 (p2 |> Quantity.minus p1)

        u2 =
            Quantity.multiplyBy 3 (p3 |> Quantity.minus p2)

        u3 =
            Quantity.multiplyBy 3 (p4 |> Quantity.minus p3)
    in
    Interval.hull3 u1 u2 u3


secondDerivativeBoundingBox : CubicSpline1d units -> Interval Float units
secondDerivativeBoundingBox spline =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        p4 =
            fourthControlPoint spline

        u1 =
            p2 |> Quantity.minus p1

        u2 =
            p3 |> Quantity.minus p2

        u3 =
            p4 |> Quantity.minus p3

        v1 =
            Quantity.multiplyBy 6 (u2 |> Quantity.minus u1)

        v2 =
            Quantity.multiplyBy 6 (u3 |> Quantity.minus u2)
    in
    Interval.from v1 v2
