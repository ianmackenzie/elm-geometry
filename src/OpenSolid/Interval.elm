module OpenSolid.Interval
  ( Interval
  , endpoints
  , width
  , interpolated
  , median
  ) where


type alias Interval =
  { lowerBound : Float
  , upperBound : Float
  }


endpoints: Interval -> (Float, Float)
endpoints interval =
  (interval.lowerBound, interval.upperBound)


width: Interval -> Float
width interval =
  interval.upperBound - interval.lowerBound


interpolated: Interval -> Float -> Float
interpolated interval parameter =
  interval.lowerBound + parameter * (interval.upperBound - interval.lowerBound)


median: Interval -> Float
median interval =
  interval.lowerBound + 0.5 * (interval.upperBound - interval.lowerBound)
