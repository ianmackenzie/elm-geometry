module Length exposing
    ( Length, Meters
    , meters, inMeters
    , microns, inMicrons, millimeters, inMillimeters, centimeters, inCentimeters, kilometers, inKilometers
    , thou, inThou, inches, inInches, feet, inFeet, yards, inYards, miles, inMiles
    , astronomicalUnits, inAstronomicalUnits, parsecs, inParsecs, lightYears, inLightYears
    )

{-| A `Length` represents a length in meters, feet, centimeters, miles etc. It
is stored as a number of meters.

@docs Length, Meters


## Metric

@docs meters, inMeters
@docs microns, inMicrons, millimeters, inMillimeters, centimeters, inCentimeters, kilometers, inKilometers


## Imperial

@docs thou, inThou, inches, inInches, feet, inFeet, yards, inYards, miles, inMiles


## Astronomical

@docs astronomicalUnits, inAstronomicalUnits, parsecs, inParsecs, lightYears, inLightYears

-}

import Quantity exposing (Quantity(..))


{-| -}
type Meters
    = Meters


{-| -}
type alias Length =
    Quantity Float Meters


{-| Construct a length from a number of meters.

    height : Length
    height =
        Length.meters 2

-}
meters : Float -> Length
meters numMeters =
    Quantity numMeters


{-| Convert a length to a number of meters.

    Length.feet 1 |> Length.inMeters
    --> 0.3048

-}
inMeters : Length -> Float
inMeters (Quantity numMeters) =
    numMeters


{-| Construct a length from a number of microns (micrometers).

    Length.microns 1
    --> Length.meters 1e-6

-}
microns : Float -> Length
microns numMicrons =
    meters (1.0e-6 * numMicrons)


{-| Convert a length to a number of microns (micrometers).

    Length.millimeters 1 |> Length.inMicrons
    --> 1000

-}
inMicrons : Length -> Float
inMicrons length =
    1.0e6 * inMeters length


{-| Construct a length from number of millimeters.
-}
millimeters : Float -> Length
millimeters numMillimeters =
    meters (0.001 * numMillimeters)


{-| Convert a length to number of millimeters.
-}
inMillimeters : Length -> Float
inMillimeters length =
    1000 * inMeters length


{-| Construct a length from a number of thou (thousandths of an inch).

    Length.thou 5
    --> Length.inches 0.005

-}
thou : Float -> Length
thou numThou =
    meters (0.0000254 * numThou)


{-| Convert a length to a number of thou (thousandths of an inch).

    Length.millimeters 1 |> Length.inThou
    --> 39.37007874015748

-}
inThou : Length -> Float
inThou length =
    inMeters length / 0.0000254


{-| Construct a length from a number of inches.
-}
inches : Float -> Length
inches numInches =
    meters (0.0254 * numInches)


{-| Convert a length to a number of inches.
-}
inInches : Length -> Float
inInches length =
    inMeters length / 0.0254


{-| Construct a length from a number of centimeters.
-}
centimeters : Float -> Length
centimeters numCentimeters =
    meters (0.01 * numCentimeters)


{-| Convert a length to a number of centimeters.
-}
inCentimeters : Length -> Float
inCentimeters length =
    100 * inMeters length


{-| Construct a length from a number of feet.
-}
feet : Float -> Length
feet numFeet =
    meters (0.3048 * numFeet)


{-| Convert a length to a number of feet.
-}
inFeet : Length -> Float
inFeet length =
    inMeters length / 0.3048


{-| Construct a length from a number of yards.
-}
yards : Float -> Length
yards numYards =
    meters (0.9144 * numYards)


{-| Convert a length to a number of yards.
-}
inYards : Length -> Float
inYards length =
    inMeters length / 0.9144


{-| Construct a length from a number of kilometers.
-}
kilometers : Float -> Length
kilometers numKilometers =
    meters (1000 * numKilometers)


{-| Convert a length to a number of kilometers.
-}
inKilometers : Length -> Float
inKilometers length =
    0.001 * inMeters length


{-| Construct a length from a number of miles.
-}
miles : Float -> Length
miles numMiles =
    meters (1609.344 * numMiles)


{-| Convert a length to a number of miles.
-}
inMiles : Length -> Float
inMiles length =
    inMeters length / 1609.344


{-| Construct a length from a number of [astronomical units][au] (AU). One AU is
approximately equal to the average distance of the Earth from the Sun.

[au]: https://en.wikipedia.org/wiki/Astronomical_unit "Astronomical unit"

-}
astronomicalUnits : Float -> Length
astronomicalUnits numAstronomicalUnits =
    meters (149597870700 * numAstronomicalUnits)


{-| Convert a length to a number of astronomical units.
-}
inAstronomicalUnits : Length -> Float
inAstronomicalUnits length =
    inMeters length / 149597870700


{-| Construct a length from a number of [parsecs][parsec].

[parsec]: https://en.wikipedia.org/wiki/Parsec "Parsec"

-}
parsecs : Float -> Length
parsecs numParsecs =
    astronomicalUnits (numParsecs * 648000 / pi)


{-| Convert a length to a number of parsecs.
-}
inParsecs : Length -> Float
inParsecs length =
    inAstronomicalUnits length * pi / 648000


{-| Construct a length from a number of light years. One light year is the
distance traveled when moving at the speed of light for one [Julian year](Duration#julianYear).
-}
lightYears : Float -> Length
lightYears numLightYears =
    meters (9460730472580800 * numLightYears)


{-| Convert a length to a number of light years.
-}
inLightYears : Length -> Float
inLightYears length =
    inMeters length / 9460730472580800
