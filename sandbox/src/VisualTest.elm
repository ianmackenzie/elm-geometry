module VisualTest exposing (Msg(..), onKeyDown, randomValue, update)

import Browser
import Browser.Events
import Json.Decode as Decode
import Random exposing (Generator)


type Msg
    = Next
    | Previous


update : Msg -> Int -> Int
update message current =
    case message of
        Next ->
            current + 1

        Previous ->
            max (current - 1) 0


onKeyDown : Sub Msg
onKeyDown =
    Browser.Events.onKeyDown
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\key ->
                    case key of
                        "ArrowLeft" ->
                            Decode.succeed Previous

                        "ArrowRight" ->
                            Decode.succeed Next

                        "ArrowUp" ->
                            Decode.succeed Previous

                        "ArrowDown" ->
                            Decode.succeed Next

                        _ ->
                            Decode.fail "Unrecognized key"
                )
        )


randomValue : Generator a -> Int -> a
randomValue generator current =
    Tuple.first (Random.step generator (Random.initialSeed current))
