module Float.Range
    exposing
        ( Range
        , Resolution
        , dropFirst
        , dropLast
        , empty
        , forEach
        , from
        , map
        , maxStepSize
        , midpoints
        , numSteps
        , numValues
        , singleton
        , toList
        )


type Resolution
    = MaxStepSize Float
    | NumSteps Int


type Range
    = Range Float Float Int
    | Singleton Float
    | Empty


empty : Range
empty =
    Empty


singleton : Float -> Range
singleton =
    Singleton


from : Float -> Float -> Resolution -> Range
from start end resolution =
    case resolution of
        NumSteps givenNumSteps ->
            if givenNumSteps > 0 then
                Range start end givenNumSteps
            else if givenNumSteps == 0 && start == end then
                Singleton start
            else
                Empty

        MaxStepSize givenMaxStepSize ->
            if start == end && givenMaxStepSize >= 0 then
                Singleton start
            else
                let
                    width =
                        abs (end - start)
                in
                if givenMaxStepSize >= width then
                    Range start end 1
                else if givenMaxStepSize > 0 then
                    -- Note that we must have width > givenMaxStepSize since the
                    -- initial givenMaxStepSize >= width check failed - therefore
                    -- the number of steps computed here will be > 1
                    Range start end (ceiling (width / givenMaxStepSize))
                else
                    Empty


numValues : Range -> Int
numValues range =
    case range of
        Range _ _ numSteps_ ->
            numSteps_ + 1

        Singleton _ ->
            1

        Empty ->
            0


map : (Float -> a) -> Range -> List a
map function range =
    case range of
        Singleton value ->
            [ function value ]

        Range start end numSteps_ ->
            countdown
                (numSteps_ - 1)
                start
                ((end - start) / toFloat numSteps_)
                function
                [ function end ]

        Empty ->
            []


forEach : Range -> (Float -> a) -> List a
forEach range function =
    map function range


countdown : Int -> Float -> Float -> (Float -> a) -> List a -> List a
countdown index start stepSize function accumulated =
    if index == 0 then
        function start :: accumulated
    else
        countdown
            (index - 1)
            start
            stepSize
            function
            (function (start + toFloat index * stepSize) :: accumulated)


toList : Range -> List Float
toList range =
    map identity range


{-| -}
numSteps : Int -> Resolution
numSteps =
    NumSteps


{-| -}
maxStepSize : Float -> Resolution
maxStepSize =
    MaxStepSize


dropFirst : Range -> Range
dropFirst range =
    case range of
        Range start_ end_ numSteps_ ->
            if numSteps_ == 1 then
                Singleton end_
            else
                Range
                    (start_ + (end_ - start_) / toFloat numSteps_)
                    end_
                    (numSteps_ - 1)

        Singleton value ->
            Empty

        Empty ->
            Empty


dropLast : Range -> Range
dropLast range =
    case range of
        Range start_ end_ numSteps_ ->
            if numSteps_ == 1 then
                Singleton start_
            else
                Range
                    start_
                    (end_ + (start_ - end_) / toFloat numSteps_)
                    (numSteps_ - 1)

        Singleton value ->
            Empty

        Empty ->
            Empty


midpoints : Range -> Range
midpoints range =
    case range of
        Range start_ end_ numSteps_ ->
            if numSteps_ == 1 then
                Singleton (start_ + (end_ - start_) / 2)
            else
                let
                    stepSize =
                        (end_ - start_) / toFloat numSteps_
                in
                Range
                    (start_ + stepSize / 2)
                    (end_ - stepSize / 2)
                    (numSteps_ - 1)

        Singleton value ->
            Empty

        Empty ->
            Empty
