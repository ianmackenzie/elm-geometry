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


interpolated: Float -> Interval -> Float
interpolated parameter interval =
  interval.lowerBound + parameter * width interval


median: Interval -> Float
median =
  interpolated 0.5
