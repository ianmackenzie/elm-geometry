module Geometry.SweptAngle
    exposing
        ( SweptAngle
        , largeNegative
        , largePositive
        , smallNegative
        , smallPositive
        )

{-|

@docs SweptAngle, smallPositive, smallNegative, largePositive, largeNegative

-}

import Geometry.Internal as Internal


type alias SweptAngle =
    Internal.SweptAngle


smallPositive : SweptAngle
smallPositive =
    Internal.SmallPositive


smallNegative : SweptAngle
smallNegative =
    Internal.SmallNegative


largePositive : SweptAngle
largePositive =
    Internal.LargePositive


largeNegative : SweptAngle
largeNegative =
    Internal.LargeNegative
