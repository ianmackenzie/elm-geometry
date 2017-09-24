module OpenSolid.ArcLengthParameterization
    exposing
        ( ArcLengthParameterization
        , Config
        , arcLength
        , build
        , parameterValue
        )

import OpenSolid.Scalar as Scalar


type ArcLengthParameterization
    = ArcLengthParameterization Float SegmentTree


type alias Config a =
    { bisect : a -> ( a, a )
    , lengthBounds : a -> ( Float, Float )
    }


type SegmentTree
    = Node
        { lengthAtStart : Float
        , leftBranch : SegmentTree
        , rightBranch : SegmentTree
        }
    | Leaf
        { lengthAtStart : Float
        , paramAtStart : Float
        , lengthAtEnd : Float
        , paramAtEnd : Float
        }


build : Config c -> Float -> c -> ArcLengthParameterization
build config tolerance curve =
    buildSegment config tolerance curve 0 0 1


buildSegment : Config c -> Float -> c -> Float -> Float -> Float -> ArcLengthParameterization
buildSegment config tolerance curve lengthAtStart paramAtStart paramAtEnd =
    let
        ( lowerBound, upperBound ) =
            config.lengthBounds curve

        maxError =
            (upperBound - lowerBound) / 2
    in
    if maxError <= tolerance || tolerance <= 0 || paramAtStart == paramAtEnd then
        let
            segmentLength =
                lowerBound + maxError

            lengthAtEnd =
                lengthAtStart + segmentLength

            leaf =
                Leaf
                    { lengthAtStart = lengthAtStart
                    , paramAtStart = paramAtStart
                    , lengthAtEnd = lengthAtEnd
                    , paramAtEnd = paramAtEnd
                    }
        in
        ArcLengthParameterization lengthAtEnd leaf
    else
        let
            ( leftCurve, rightCurve ) =
                config.bisect curve

            paramAtSplit =
                paramAtStart + 0.5 * (paramAtEnd - paramAtStart)

            (ArcLengthParameterization lengthAtLeftEnd leftBranch) =
                buildSegment config
                    (0.5 * tolerance)
                    leftCurve
                    lengthAtStart
                    paramAtStart
                    paramAtSplit

            (ArcLengthParameterization lengthAtEnd rightBranch) =
                buildSegment config
                    (0.5 * tolerance)
                    rightCurve
                    lengthAtLeftEnd
                    paramAtSplit
                    paramAtEnd

            node =
                Node
                    { lengthAtStart = lengthAtStart
                    , leftBranch = leftBranch
                    , rightBranch = rightBranch
                    }
        in
        ArcLengthParameterization lengthAtEnd node


arcLength : ArcLengthParameterization -> Float
arcLength (ArcLengthParameterization arcLength _) =
    arcLength


parameterValue : ArcLengthParameterization -> Float -> Maybe Float
parameterValue (ArcLengthParameterization arcLength segmentTree) s =
    if s >= 0 && s <= arcLength then
        Just (unsafeParameterValue segmentTree s)
    else
        Nothing


unsafeParameterValue : SegmentTree -> Float -> Float
unsafeParameterValue tree s =
    case tree of
        Leaf { lengthAtStart, paramAtStart, lengthAtEnd, paramAtEnd } ->
            let
                lengthFraction =
                    (s - lengthAtStart) / (lengthAtEnd - lengthAtStart)
            in
            Scalar.interpolateFrom paramAtStart paramAtEnd lengthFraction

        Node { leftBranch, rightBranch } ->
            if s < lengthAtStart rightBranch then
                unsafeParameterValue leftBranch s
            else
                unsafeParameterValue rightBranch s


lengthAtStart : SegmentTree -> Float
lengthAtStart tree =
    case tree of
        Node { lengthAtStart } ->
            lengthAtStart

        Leaf { lengthAtStart } ->
            lengthAtStart
