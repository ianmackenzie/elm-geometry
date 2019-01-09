module Quantity exposing
    ( Quantity(..)
    , Squared, Cubed, Product, Rate
    , zero, infinity, positiveInfinity, negativeInfinity
    , lessThan, greaterThan, lessThanOrEqualTo, greaterThanOrEqualTo, compare, equalWithin, max, min, isNaN, isInfinite
    , negate, abs, plus, minus, multiplyBy, divideBy, squared, sqrt, cubed, cbrt
    , times, over, over_
    , per, at, at_, for, inverse
    , ratio, clamp, interpolateFrom
    , round, floor, ceiling, truncate, toFloatQuantity
    , sum, minimum, maximum, sort, sortBy
    , Unitless, int, toInt, float, toFloat
    )

{-|

@docs Quantity


# Unit types

The `Squared`, `Cubed`, `Product` and `Rate` units types allow you to build up
and work with composite units in a fairly flexible way.

@docs Squared, Cubed, Product, Rate


# Constants

@docs zero, infinity, positiveInfinity, negativeInfinity


# Comparison

@docs lessThan, greaterThan, lessThanOrEqualTo, greaterThanOrEqualTo, compare, equalWithin, max, min, isNaN, isInfinite


# Arithmetic

@docs negate, abs, plus, minus, multiplyBy, divideBy, squared, sqrt, cubed, cbrt


## Working with products

@docs times, over, over_


## Working with rates

@docs per, at, at_, for, inverse


## Miscellaneous

@docs ratio, clamp, interpolateFrom


# `Int`/`Float` conversion

These functions only really make sense for quantities in units like pixels,
cents or game tiles where an `Int` number of units is meaningful. For quantities
like `Length` or `Duration`, it doesn't really make sense to round to an `Int`
value since the underyling base unit is pretty arbitrary - should `round`ing a
`Duration` give you an `Int` number of seconds, milliseconds, or something else?

@docs round, floor, ceiling, truncate, toFloatQuantity


# List functions

These functions act just like the corresponding functions in the built-in `List`
module. They're necessary because the built-in `List.sum` only supports `List
Int` and `List Float`, and `minimum`/`maximum`/`sort` only support built-in
comparable types like `Int`, `Float`, `String` and tuples.

@docs sum, minimum, maximum, sort, sortBy


# Unitless quantities

It is sometimes useful to be able to represent _unitless_ quantities, especially
when working with generic code (in most other cases, it is likely simpler and
easier to just use `Int` or `Float` values directly). All the conversions in
this section simply wrap or unwrap a `Float` or `Int` value into a `Quantity`
value, and so should get compiled away entirely when using `elm make
--optimize`.

@docs Unitless, int, toInt, float, toFloat

-}


{-| A `Quantity` is effectively a `number` (an `Int` or `Float`) tagged with a
`units` type. So a

    Quantity Float Meters

is a `Float` number of `Meters` and a

    Quantity Int Pixels

is an `Int` number of `Pixels`. When compiling with `elm make --optimize` the
`Quantity` wrapper type will be compiled away, so the runtime performance should
be comparable to using a raw `Float` or `Int`.

-}
type Quantity number units
    = Quantity number



---------- UNIT TYPES ----------


{-| Represents a units type that is the square of some other units type; for
example, `Meters` is one units type (the units type of a [`Length`](Length)) and
`Squared Meters` is another (the units type of an [`Area`](Area)). See the
[`squared`](#squared) and [`sqrt`](#sqrt) functions for examples of use.

This is a special case of the `Product` units type.

-}
type alias Squared units =
    Product units units


{-| Represents a units type that is the cube of some other units type; for
example, `Meters` is one units type (the units type of a [`Length`](Length)) and
`Cubed Meters` is another (the units type of an [`Volume`](Volume)). See the
[`cubed`](Quantity#cubed) and [`cbrt`](Quantity#cbrt) functions for examples of
use.

This is a special case of the `Product` units type.

-}
type alias Cubed units =
    Product (Product units units) units


{-| Represents a units type that is the product of two other units types. This
is a more general form of `Squared` or `Cubed`. See [`times`](#product),
[`over`](#over) and [`over_`](#over_) for how it can be used.
-}
type Product units1 units2
    = Product units1 units2


{-| Represents the units type of a rate or quotient such as a speed (`Rate
Meters Seconds`) or a pressure (`Rate Newtons SquareMeters`). See [Working with
rates](#working-with-rates) for details.
-}
type Rate dependentUnits independentUnits
    = Rate dependentUnits independentUnits



---------- CONSTANTS ----------


{-| A generic zero value. This can be treated as either an `Int` or `Float`
quantity in any units type, similar to how `Nothing` can be treated as any kind
of `Maybe` type and `[]` can be treated as any kind of `List`.
-}
zero : Quantity number units
zero =
    Quantity 0


{-| Alias for `positiveInfinity`.
-}
infinity : Quantity Float units
infinity =
    positiveInfinity


{-| A generic positive infinity value.
-}
positiveInfinity : Quantity Float units
positiveInfinity =
    Quantity (1 / 0)


{-| A generic negative infinity value.
-}
negativeInfinity : Quantity Float units
negativeInfinity =
    Quantity (-1 / 0)



---------- COMPARISON ----------


{-| Check if one quantity is less than another. Note the [argument order](/#argument-order)!

    oneMeter =
        Length.meters 1

    Length.feet 1 |> Quantity.lessThan oneMeter
    --> True

    -- Same as:
    Quantity.lessThan oneMeter (Length.feet 1)
    --> True

    List.filter (Quantity.lessThan oneMeter)
        [ Length.feet 1
        , Length.parsecs 1
        , Length.yards 1
        , Length.lightYears 1
        ]
    --> [ Length.feet 1, Length.yards 1 ]

-}
lessThan : Quantity number units -> Quantity number units -> Bool
lessThan (Quantity y) (Quantity x) =
    x < y


{-| Check if one quantity is greater than another. Note the [argument order](/#argument-order)!

    oneMeter =
        Length.meters 1

    Length.feet 1 |> Quantity.greaterThan oneMeter
    --> False

    -- Same as:
    Quantity.greaterThan oneMeter (Length.feet 1)
    --> False

    List.filter (Quantity.greaterThan oneMeter)
        [ Length.feet 1
        , Length.parsecs 1
        , Length.yards 1
        , Length.lightYears 1
        ]
    --> [ Length.parsecs 1, Length.lightYears 1 ]

-}
greaterThan : Quantity number units -> Quantity number units -> Bool
greaterThan (Quantity y) (Quantity x) =
    x > y


{-| Check if one quantity is less than or equal to another. Note the [argument
order](/#argument-order)!
-}
lessThanOrEqualTo : Quantity number units -> Quantity number units -> Bool
lessThanOrEqualTo (Quantity y) (Quantity x) =
    x <= y


{-| Check if one quantity is greater than or equal to another. Note the
[argument order](/#argument-order)!
-}
greaterThanOrEqualTo : Quantity number units -> Quantity number units -> Bool
greaterThanOrEqualTo (Quantity y) (Quantity x) =
    x >= y


{-| Compare two quantities, returning an [`Order`](https://package.elm-lang.org/packages/elm/core/latest/Basics#Order)
value indicating whether the first is less than, equal to or greater than the
second.

    Quantity.compare (Duration.minutes 90) (Duration.hours 1)
    --> GT

    Quantity.compare (Duration.minutes 60) (Duration.hours 1)
    --> EQ

-}
compare : Quantity number units -> Quantity number units -> Order
compare (Quantity x) (Quantity y) =
    Basics.compare x y


{-| Check if two quantities are equal within a given absolute tolerance. The
given tolerance must be greater than or equal to zero - if it is negative, then
the result will always be false.

    -- 3 feet is 91.44 centimeters or 0.9144 meters

    Quantity.equalWithin (Length.centimeters 10)
        (Length.meters 1)
        (Length.feet 3)
    --> True

    Quantity.equalWithin (Length.centimeters 5)
        (Length.meters 1)
        (Length.feet 3)
    --> False

-}
equalWithin : Quantity number units -> Quantity number units -> Quantity number units -> Bool
equalWithin (Quantity tolerance) (Quantity x) (Quantity y) =
    Basics.abs (x - y) <= tolerance


{-| Find the maximum of two quantities.

    Quantity.max (Duration.hours 2) (Duration.minutes 100)
    --> Duration.hours 2

-}
max : Quantity number units -> Quantity number units -> Quantity number units
max (Quantity x) (Quantity y) =
    Quantity (Basics.max x y)


{-| Find the minimum of two quantities.

    Quantity.min (Duration.hours 2) (Duration.minutes 100)
    --> Duration.minutes 100

-}
min : Quantity number units -> Quantity number units -> Quantity number units
min (Quantity x) (Quantity y) =
    Quantity (Basics.min x y)


{-| Check if a quantity is positive or negative infinity.

    Quantity.isInfinite
        (Length.meters 1
            |> Quantity.per (Duration.seconds 0)
        )
    --> True

    Quantity.isInfinite Quantity.negativeInfinity
    --> True

-}
isInfinite : Quantity Float units -> Bool
isInfinite (Quantity value) =
    Basics.isInfinite value


{-| Check if a quantity's underlying value is NaN (not-a-number).

    Quantity.isNan (Quantity.sqrt (Area.squareMeters -4))
    --> True

    Quantity.isNan (Quantity.sqrt (Area.squareMeters 4))
    --> False

-}
isNaN : Quantity Float units -> Bool
isNaN (Quantity value) =
    Basics.isNaN value



---------- ARITHMETIC ----------


{-| Negate a quantity!

    Quantity.negate (Length.millimeters 10)
    --> Length.millimeters -10

-}
negate : Quantity number units -> Quantity number units
negate (Quantity value) =
    Quantity -value


{-| Add two quantities.

    Length.meters 1 |> Quantity.plus (Length.centimeters 5)
    --> Length.centimeters 105

-}
plus : Quantity number units -> Quantity number units -> Quantity number units
plus (Quantity y) (Quantity x) =
    Quantity (x + y)


{-| Subtract one quantity from another. Note the [argument order](/#argument-order)!

    fifteenMinutes =
        Duration.minutes 15

    Duration.hours 1 |> Quantity.minus fifteenMinutes
    --> Duration.minutes 45

    -- Same as:
    Quantity.minus fifteenMinutes (Duration.hours 1)
    --> Duration.minutes 45

    List.map (Quantity.minus fifteenMinutes)
        [ Duration.hours 1
        , Duration.hours 2
        , Duration.minutes 30
        ]
    --> [ Duration.minutes 45
    --> , Duration.minutes 105
    --> , Duration.minutes 15
    --> ]

-}
minus : Quantity number units -> Quantity number units -> Quantity number units
minus (Quantity y) (Quantity x) =
    Quantity (x - y)


{-| Multiply two quantities with units types `units1` and `units2` together,
resulting in a quantity with units type `Product units1 units2`. Note the
[argument order](/#argument-order)!

This works for any two units types, but one special case is worth pointing out.
The units type of an [`Area`](Area) is `SquareMeters`, which is a type alias for
`Squared Meters`, which in turn expands to `Product Meters Meters`. This means
that the product of two `Length`s does in fact give you an `Area`:

    -- This is the definition of an acre, I kid you not 😈
    Length.feet 66 |> Quantity.times (Length.feet 660)
    --> Area.acres 1

We can also multiply an `Area` by a `Length` to get a `Volume`:

    Area.squareMeters 1
        |> Quantity.times
            (Length.centimeters 1)
    --> Volume.liters 10

Note that there are [other forms of multiplication](/#multiplication-and-division)!

-}
times : Quantity number units2 -> Quantity number units1 -> Quantity number (Product units1 units2)
times (Quantity y) (Quantity x) =
    Quantity (x * y)


{-| Divide a quantity in `Product units1 units2` by a quantity in `units1`,
resulting in another quantity in `units2`. For example, the units type of a
`Force` is `Product Kilograms MetersPerSecondSquared` (mass times acceleration),
so we could divide a force by a given mass to determine how fast that mass would
be accelerated by the given force:

    Force.newtons 100
        |> Quantity.over
            (Mass.kilograms 50)
    --> Acceleration.metersPerSecondSquared 2

Note that there are [other forms of division](/#multiplication-and-division)!

-}
over : Quantity Float units1 -> Quantity Float (Product units1 units2) -> Quantity Float units2
over (Quantity y) (Quantity x) =
    Quantity (x / y)


{-| Just like `over` but divide by a quantity in `units2`, resulting in another
quantity in `units1`. For example, we could divide a force by a desired
acceleration to determine how much mass could be accelerated at that rate:

    Force.newtons 100
        |> Quantity.over_
            (Acceleration.metersPerSecondSquared 5)
    --> Mass.kilograms 20

-}
over_ : Quantity Float units2 -> Quantity Float (Product units1 units2) -> Quantity Float units1
over_ (Quantity y) (Quantity x) =
    Quantity (x / y)


{-| Find the ratio of two quantities with the same units.

    Quantity.ratio (Length.miles 1) (Length.yards 1)
    --> 1760

-}
ratio : Quantity Float units -> Quantity Float units -> Float
ratio (Quantity x) (Quantity y) =
    x / y


{-| Scale a `Quantity` by a `number`.

    Quantity.multiplyBy 1.5 (Duration.hours 1)
    --> Duration.minutes 90

Note that there are [other forms of multiplication](/#multiplication-and-division)!

-}
multiplyBy : number -> Quantity number units -> Quantity number units
multiplyBy scale (Quantity value) =
    Quantity (scale * value)


{-| Divide a `Quantity` by a `Float`.

    Quantity.divideBy 2 (Duration.hours 1)
    --> Duration.minutes 30

Note that there are [other forms of division](/#multiplication-and-division)!

-}
divideBy : Float -> Quantity Float units -> Quantity Float units
divideBy divisor (Quantity value) =
    Quantity (value / divisor)


{-| Get the absolute value of a quantity.

    Quantity.abs (Duration.milliseconds -10)
    --> Duration.milliseconds 10

-}
abs : Quantity number units -> Quantity number units
abs (Quantity value) =
    Quantity (Basics.abs value)


{-| Given a lower and upper bound, clamp a given quantity to within those
bounds. Say you wanted to clamp an angle to be between +/-30 degrees:

    lowerBound =
        Angle.degrees -30

    upperBound =
        Angle.degrees 30

    Quantity.clamp lowerBound upperBound (Angle.degrees 5)
    --> Angle.degrees 5

    -- One radian is approximately 57 degrees
    Quantity.clamp lowerBound upperBound (Angle.radians 1)
    --> Angle.degrees 30

    Quantity.clamp lowerBound upperBound (Angle.turns -0.5)
    --> Angle.degrees -30

-}
clamp : Quantity number units -> Quantity number units -> Quantity number units -> Quantity number units
clamp (Quantity lower) (Quantity upper) (Quantity value) =
    if lower <= upper then
        Quantity (Basics.clamp lower upper value)

    else
        Quantity (Basics.clamp upper lower value)


{-| Square a quantity with some `units`, resulting in a new quantity in
`Squared units`:

    Quantity.squared (Length.meters 5)
    --> Area.squareMeters 25

-}
squared : Quantity number units -> Quantity number (Squared units)
squared (Quantity value) =
    Quantity (value * value)


{-| Take a quantity in `Squared units` and return the square root of that
quantity in plain `units`:

    Quantity.sqrt (Area.hectares 1)
    --> Length.meters 100

Getting fancier, you could write a 2D hypotenuse (magnitude) function that
worked on _any_ quantity type (length, speed, force...) as

    hypotenuse :
        Quantity Float units
        -> Quantity Float units
        -> Quantity Float units
    hypotenuse x y =
        Quantity.sqrt
            (Quantity.squared x
                |> Quantity.plus
                    (Quantity.squared y)
            )

This works because:

  - The `x` and `y` arguments are both in `units`
  - So each squared item is in `Squared units`
  - So the sum is also in `Squared units`
  - And calling `sqrt` on something in `Squared units` returns a value back in
    `units`

-}
sqrt : Quantity Float (Squared units) -> Quantity Float units
sqrt (Quantity value) =
    Quantity (Basics.sqrt value)


{-| Cube a quantity with some `units`, resulting in a new quantity in
`Cubed units`.

    Quantity.cubed (Length.meters 5)
    --> Volume.cubicMeters 125

-}
cubed : Quantity number units -> Quantity number (Cubed units)
cubed (Quantity value) =
    Quantity (value * value * value)


{-| Take a quantity in `Cubed units` and return the cube root of that
quantity in plain `units`.

    Quantity.cbrt (Volume.liters 1)
    --> Length.centimeters 10

-}
cbrt : Quantity Float (Cubed units) -> Quantity Float units
cbrt (Quantity value) =
    if value >= 0 then
        Quantity (value ^ (1 / 3))

    else
        Quantity -(-value ^ (1 / 3))


{-| Interpolate from the first quantity to the second, based on a parameter that
ranges from zero to one. Passing a parameter value of zero will return the start
value and passing a parameter value of one will return the end value.

    fiveMeters =
        Length.meters 5

    tenMeters =
        Length.meters 10

    Quantity.interpolateFrom fiveMeters tenMeters 0
    --> Length.meters 5

    Quantity.interpolateFrom fiveMeters tenMeters 1
    --> Length.meters 10

    Quantity.interpolateFrom fiveMeters tenMeters 0.6
    --> Length.meters 8

The end value can be less than the start value:

    Quantity.interpolateFrom tenMeters fiveMeters 0.1
    --> Length.meters 9.5

Parameter values less than zero or greater than one can be used to extrapolate:

    Quantity.interpolateFrom fiveMeters tenMeters 1.5
    --> Length.meters 12.5

    Quantity.interpolateFrom fiveMeters tenMeters -0.5
    --> Length.meters 2.5

    Quantity.interpolateFrom tenMeters fiveMeters -0.2
    --> Length.meters 11

-}
interpolateFrom : Quantity Float units -> Quantity Float units -> Float -> Quantity Float units
interpolateFrom (Quantity start) (Quantity end) parameter =
    if parameter <= 0.5 then
        Quantity (start + parameter * (end - start))

    else
        Quantity (end + (1 - parameter) * (start - end))



---------- INT/FLOAT CONVERSIONS ----------


{-| Round a `Float`-valued quantity to the nearest `Int`.

    Quantity.round (Pixels.pixels 3.5)
    --> Pixels.pixels 4

-}
round : Quantity Float units -> Quantity Int units
round (Quantity value) =
    Quantity (Basics.round value)


{-| Round a `Float`-valued quantity down to the nearest `Int`.

    Quantity.floor (Pixels.pixels 2.9)
    --> Pixels.pixels 2

    Quantity.floor (Pixels.pixels -2.1)
    --> Pixels.pixels -3

-}
floor : Quantity Float units -> Quantity Int units
floor (Quantity value) =
    Quantity (Basics.floor value)


{-| Round a `Float`-valued quantity up to the nearest `Int`.

    Quantity.ceiling (Pixels.pixels 1.2)
    --> Pixels.pixels 2

    Quantity.ceiling (Pixels.pixels -2.1)
    --> Pixels.pixels -2

-}
ceiling : Quantity Float units -> Quantity Int units
ceiling (Quantity value) =
    Quantity (Basics.ceiling value)


{-| Round a `Float`-valued quantity towards zero.

    Quantity.truncate (Pixels.pixels -2.8)
    --> Pixels.pixels -2

-}
truncate : Quantity Float units -> Quantity Int units
truncate (Quantity value) =
    Quantity (Basics.truncate value)


{-| Convert a `Quantity Int units` to a `Quantity Float units` with the same
value. Useful when you have an `Int`-valued quantity and want to divide it by
something, multiply it by a fractional value etc.
-}
toFloatQuantity : Quantity Int units -> Quantity Float units
toFloatQuantity (Quantity value) =
    Quantity (Basics.toFloat value)



---------- LIST FUNCTIONS ----------


{-| Find the sum of a list of quantities.

    Quantity.sum
        [ Length.meters 1
        , Length.centimeters 2
        , Length.millimeters 3
        ]
    --> Length.meters 1.023

    Quantity.sum []
    --> Quantity.zero

-}
sum : List (Quantity number units) -> Quantity number units
sum quantities =
    List.foldl plus zero quantities


{-| Find the minimum value in a list of quantities. Returns `Nothing` if the
list is empty.

    Quantity.minimum
        [ Mass.kilograms 1
        , Mass.pounds 2
        , Mass.tonnes 3
        ]
    --> Just (Mass.pounds 2)

-}
minimum : List (Quantity number units) -> Maybe (Quantity number units)
minimum quantities =
    case quantities of
        [] ->
            Nothing

        first :: rest ->
            Just (List.foldl min first rest)


{-| Find the maximum value in a list of quantities. Returns `Nothing` if the
list is empty.

    Quantity.maximum
        [ Mass.kilograms 1
        , Mass.pounds 2
        , Mass.tonnes 3
        ]
    --> Just (Mass.tonnes 3)

-}
maximum : List (Quantity number units) -> Maybe (Quantity number units)
maximum quantities =
    case quantities of
        [] ->
            Nothing

        first :: rest ->
            Just (List.foldl max first rest)


unwrap : Quantity number units -> number
unwrap (Quantity value) =
    value


{-| Sort a list of quantities.

    Quantity.sort
        [ Mass.kilograms 1
        , Mass.pounds 2
        , Mass.tonnes 3
        ]
    --> [ Mass.pounds 2
    --> , Mass.kilograms 1
    --> , Mass.tonnes 3
    --> ]

-}
sort : List (Quantity number units) -> List (Quantity number units)
sort quantities =
    List.sortBy unwrap quantities


{-| Sort an arbitrary list of values by a derived `Quantity`. If you had

    people =
        [ { name = "Bob", height = Length.meters 1.6 }
        , { name = "Charlie", height = Length.meters 2.0 }
        , { name = "Alice", height = Length.meters 1.8 }
        ]

then you could sort by name with

    List.sortBy .name people
    --> [ { name = "Alice", height = Length.meters 1.8 }
    --> , { name = "Bob", height = Length.meters 1.6 }
    --> , { name = "Charlie", height = Length.meters 2.0 }
    --> ]

(nothing new there!), and sort by height with

    Quantity.sortBy .height people
    --> [ { name = "Bob", height = Length.meters 1.6 }
    --> , { name = "Alice", height = Length.meters 1.8 }
    --> , { name = "Charlie", height = Length.meters 2.0 }
    --> ]

-}
sortBy : (a -> Quantity number units) -> List a -> List a
sortBy toQuantity list =
    let
        comparator first second =
            compare (toQuantity first) (toQuantity second)
    in
    List.sortWith comparator list



---------- WORKING WITH RATES ----------


{-| Construct a rate of change by dividing a dependent quantity (numerator) by
an independent quantity (denominator):

    distance =
        Length.miles 1

    time =
        Duration.minutes 1

    speed =
        distance |> Quantity.per time

    speed |> Speed.inMilesPerHour
    --> 60

Note that we could directly use our rate of change value as a `Speed`! That is
because many built-in quantity types are defined as rates of change, for
example:

  - `Speed` is `Length` per `Duration`
  - `Acceleration` is `Speed` per `Duration`
  - `Pressure` is `Force` per `Area`
  - `Power` is `Energy` per `Duration`
  - `Current` is `Charge` per `Duration`
  - `Resistance` is `Voltage` per `Current`
  - `Voltage` is `Power` per `Current`

Note that there are [other forms of division](/#multiplication-and-division)!

-}
per : Quantity Float independentUnits -> Quantity Float dependentUnits -> Quantity Float (Rate dependentUnits independentUnits)
per (Quantity independentValue) (Quantity dependentValue) =
    Quantity (dependentValue / independentValue)


{-| Multiply a rate of change by an independent quantity (the denominator in
the rate) to get a total value:

    Duration.minutes 30
        |> Quantity.at
            (Speed.kilometersPerHour 100)
    --> Length.kilometers 50

Can be useful to define conversion functions from one unit to another, since
if you define a `rate` then `Quantity.at rate` will give you a conversion
function:

    pixelDensity : Quantity Float (Rate Pixels Meters)
    pixelDensity =
        Pixels.pixels 96 |> Quantity.per (Length.inches 1)

    lengthToPixels : Length -> Quantity Float Pixels
    lengthToPixels length =
        Quantity.at pixelDensity length

    lengthToPixels (Length.inches 3)
    --> Pixels.pixels 288

Eagle-eyed readers will note that using partial application you could also
simply write

    lengthToPixels =
        Quantity.at pixelDensity

Note that there are [other forms of multiplication](/#multiplication-and-division)!

-}
at : Quantity number (Rate dependentUnits independentUnits) -> Quantity number independentUnits -> Quantity number dependentUnits
at (Quantity rate) (Quantity independentValue) =
    Quantity (rate * independentValue)


{-| Given a rate and a _dependent_ quantity (total value), determine the
necessary amount of the _independent_ quantity:

    Length.kilometers 75
        |> Quantity.at_
            (Speed.kilometersPerHour 100)
    --> Duration.minutes 45

Where `at` performs multiplication, `at_` performs division - you multiply a
speed by a duration to get a distance, but you divide a distance by a speed to
get a duration.

Similar to `at`, `at_` can be used to define an _inverse_ conversion function:

    pixelDensity : Quantity Float (Rate Pixels Meters)
    pixelDensity =
        Pixels.pixels 96 |> Quantity.per (Length.inches 1)

    pixelsToLength : Quantity Float Pixels -> Length
    pixelsToLength pixels =
        Quantity.at_ pixelDensity pixels

    pixelsToLength (Pixels.pixels 48)
    --> Length.inches 0.5

-}
at_ : Quantity Float (Rate dependentUnits independentUnits) -> Quantity Float dependentUnits -> Quantity Float independentUnits
at_ (Quantity rate) (Quantity dependentValue) =
    Quantity (dependentValue / rate)


{-| Same as `at` but with the argument order flipped, which may read better
in some cases:

    Speed.kilometersPerHour 100
        |> Quantity.for
            (Duration.minutes 30)
    --> Length.kilometers 50

-}
for : Quantity number independentUnits -> Quantity number (Rate dependentUnits independentUnits) -> Quantity number dependentUnits
for (Quantity independentValue) (Quantity rate) =
    Quantity (rate * independentValue)


{-| Find the inverse of a given rate. May be useful if you are using a rate to
define a conversion, and want to convert the other way;

    Quantity.at (Quantity.inverse rate)

is equivalent to

    Quantity.at_ rate

-}
inverse : Quantity Float (Rate dependentUnits independentUnits) -> Quantity Float (Rate independentUnits dependentUnits)
inverse (Quantity rate) =
    Quantity (1 / rate)



---------- UNITLESS QUANTITIES ----------


{-| A special units type representing 'no units'. A `Quantity Int Unitless`
value is interchangeable with a simple `Int`, and a `Quantity Float Unitless`
value is interchangeable with a simple `Float`.
-}
type Unitless
    = Unitless


{-| Convert a plain `Int` into a `Quantity Int Unitless` value.
-}
int : Int -> Quantity Int Unitless
int value =
    Quantity value


{-| Convert a `Quantity Int Unitless` value into a plain `Int`.
-}
toInt : Quantity Int Unitless -> Int
toInt (Quantity value) =
    value


{-| Convert a plain `Float` into a `Quantity Float Unitless` value.
-}
float : Float -> Quantity Float Unitless
float value =
    Quantity value


{-| Convert a `Quantity Float Unitless` value into a plain `Float`.

If you're looking for a function to convert a `Quantity Int units` to `Quantity
Float units`, check out [`toFloatQuantity`](#toFloatQuantity).

-}
toFloat : Quantity Float Unitless -> Float
toFloat (Quantity value) =
    value
