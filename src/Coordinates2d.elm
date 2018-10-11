module Coordinates2d exposing (Coordinates2d)

import Quantity exposing (Quantity)


type Coordinates2d system number units
    = Coordinates2d ( Quantity number units, Quantity number units )
