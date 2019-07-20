module Tests.Literals exposing (just, ok)


just : Maybe a -> a
just maybe =
    case maybe of
        Just value ->
            value

        Nothing ->
            Debug.todo "Maybe is Nothing when it should be Just"


ok : Result x a -> a
ok result =
    case result of
        Ok value ->
            value

        Err _ ->
            Debug.todo "Result is Err when it should be Ok"
