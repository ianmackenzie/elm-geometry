--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module QuadraticSpline1d exposing
    ( QuadraticSpline1d
    , at
    , at_
    , boundingBox
    , endDerivative
    , endPoint
    , firstControlPoint
    , firstDerivative
    , firstDerivativeBoundingBox
    , fromControlPoints
    , pointOn
    , secondControlPoint
    , secondDerivative
    , startDerivative
    , startPoint
    , thirdControlPoint
    )

import Geometry.Types as Types
import Quantity exposing (Quantity(..), Rate)
import Quantity.Interval as Interval exposing (Interval)


type alias QuadraticSpline1d units =
    Types.QuadraticSpline1d units


fromControlPoints : Quantity Float units -> Quantity Float units -> Quantity Float units -> QuadraticSpline1d units
fromControlPoints p1 p2 p3 =
    Types.QuadraticSpline1d
        { firstControlPoint = p1
        , secondControlPoint = p2
        , thirdControlPoint = p3
        }


at : Quantity Float (Rate units2 units1) -> QuadraticSpline1d units1 -> QuadraticSpline1d units2
at rate (Types.QuadraticSpline1d spline) =
    Types.QuadraticSpline1d
        { firstControlPoint = Quantity.at rate spline.firstControlPoint
        , secondControlPoint = Quantity.at rate spline.secondControlPoint
        , thirdControlPoint = Quantity.at rate spline.thirdControlPoint
        }


at_ : Quantity Float (Rate units1 units2) -> QuadraticSpline1d units1 -> QuadraticSpline1d units2
at_ rate spline =
    at (Quantity.inverse rate) spline


startPoint : QuadraticSpline1d units -> Quantity Float units
startPoint (Types.QuadraticSpline1d spline) =
    spline.firstControlPoint


endPoint : QuadraticSpline1d units -> Quantity Float units
endPoint (Types.QuadraticSpline1d spline) =
    spline.thirdControlPoint


firstControlPoint : QuadraticSpline1d units -> Quantity Float units
firstControlPoint (Types.QuadraticSpline1d spline) =
    spline.firstControlPoint


secondControlPoint : QuadraticSpline1d units -> Quantity Float units
secondControlPoint (Types.QuadraticSpline1d spline) =
    spline.secondControlPoint


thirdControlPoint : QuadraticSpline1d units -> Quantity Float units
thirdControlPoint (Types.QuadraticSpline1d spline) =
    spline.thirdControlPoint


startDerivative : QuadraticSpline1d units -> Quantity Float units
startDerivative spline =
    Quantity.twice (secondControlPoint spline |> Quantity.minus (firstControlPoint spline))


endDerivative : QuadraticSpline1d units -> Quantity Float units
endDerivative spline =
    Quantity.twice (thirdControlPoint spline |> Quantity.minus (secondControlPoint spline))


boundingBox : QuadraticSpline1d units -> Interval Float units
boundingBox spline =
    Interval.hull3
        (firstControlPoint spline)
        (secondControlPoint spline)
        (thirdControlPoint spline)


pointOn : QuadraticSpline1d units -> Float -> Quantity Float units
pointOn spline parameterValue =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        q1 =
            Quantity.interpolateFrom p1 p2 parameterValue

        q2 =
            Quantity.interpolateFrom p2 p3 parameterValue
    in
    Quantity.interpolateFrom q1 q2 parameterValue


firstDerivative : QuadraticSpline1d units -> Float -> Quantity Float units
firstDerivative spline parameterValue =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        v1 =
            p2 |> Quantity.minus p1

        v2 =
            p3 |> Quantity.minus p2
    in
    Quantity.twice (Quantity.interpolateFrom v1 v2 parameterValue)


{-| Get the bounds on the first derivative of a spline.
-}
firstDerivativeBoundingBox : QuadraticSpline1d units -> Interval Float units
firstDerivativeBoundingBox spline =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        v1 =
            Quantity.twice (p2 |> Quantity.minus p1)

        v2 =
            Quantity.twice (p3 |> Quantity.minus p2)
    in
    Interval.from v1 v2


{-| Get the second derivative of a spline (for a quadratic spline, this is a
constant).
-}
secondDerivative : QuadraticSpline1d units -> Quantity Float units
secondDerivative spline =
    let
        p1 =
            firstControlPoint spline

        p2 =
            secondControlPoint spline

        p3 =
            thirdControlPoint spline

        v1 =
            p2 |> Quantity.minus p1

        v2 =
            p3 |> Quantity.minus p2
    in
    Quantity.twice (v2 |> Quantity.minus v1)
