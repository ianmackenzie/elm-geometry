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


{-| Argument type used in [`fromEndpoints`](#fromEndpoints).
-}
type alias SweptAngle =
    Internal.SweptAngle


{-| Flag used as argument to [`fromEndpoints`](#fromEndpoints).
-}
smallPositive : SweptAngle
smallPositive =
    Internal.SmallPositive


{-| Flag used as argument to [`fromEndpoints`](#fromEndpoints).
-}
smallNegative : SweptAngle
smallNegative =
    Internal.SmallNegative


{-| Flag used as argument to [`fromEndpoints`](#fromEndpoints).
-}
largePositive : SweptAngle
largePositive =
    Internal.LargePositive


{-| Flag used as argument to [`fromEndpoints`](#fromEndpoints).
-}
largeNegative : SweptAngle
largeNegative =
    Internal.LargeNegative
