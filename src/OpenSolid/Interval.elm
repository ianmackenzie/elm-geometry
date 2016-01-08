module OpenSolid.Interval
  ( Interval(Interval)
  , endpoints
  , width
  , interpolated
  , median
  ) where


type Interval =
  Interval Float Float


endpoints: Interval -> (Float, Float)
endpoints (Interval lower upper) =
  (lower, upper)


width: Interval -> Float
width (Interval lower upper) =
  upper - lower


interpolated: Interval -> Float -> Float
interpolated (Interval lower upper) parameter =
  lower + parameter * (upper - lower)


median: Interval -> Float
median (Interval lower upper) =
  lower + 0.5 * (upper - lower)
