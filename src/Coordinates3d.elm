module Coordinates3d exposing (Coordinates3d)

import Quantity exposing (Quantity)


type Coordinates3d system number units
    = Coordinates3d ( Quantity number units, Quantity number units, Quantity number units )
