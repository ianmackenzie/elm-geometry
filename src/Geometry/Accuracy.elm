module Geometry.Accuracy exposing (Accuracy, maxError)

import Geometry.Internal as Internal


type alias Accuracy =
    Internal.Accuracy


maxError : Float -> Accuracy
maxError =
    Internal.MaxError
