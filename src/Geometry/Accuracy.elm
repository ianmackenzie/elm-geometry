module Geometry.Accuracy exposing (Accuracy, maxError)

{-| The `Accuracy` type is used to control the accuracy of approximations such
as `Arc2d.toPolyline` and `CubicSpline3d.arcLengthParameterized`.

@docs Accuracy, maxError

-}

import Geometry.Types as Types


{-| Opaque type used to specify the accuracy of some approximation. Currently
the only supported way to specify accuracy is 'maximum error', but in the future
other criteria such as 'number of approximating segments' may be added.
-}
type alias Accuracy =
    Types.Accuracy


{-| Specify the maximum error for a given approximation. For example, calling

    Arc2d.toPolyline (Accuracy.maxError 0.001) arc

will ensure that the distance between the returned polyline and the actual arc
will never be greater than 0.001 units.

-}
maxError : Float -> Accuracy
maxError =
    Types.MaxError
