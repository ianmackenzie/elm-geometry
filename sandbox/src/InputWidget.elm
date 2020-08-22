module InputWidget exposing
    ( checkbox, radioButton, lineEdit, comboBox, slider
    , custom
    )

{-| Functions for creating input widgets of the general form `a -> Html a`. You
should use `Html.map` to convert the produced messages to the message type used
by your app, and the new value should generally be stored in your model and fed
back in to the `view` function. This means that the value emitted from a given
fragment of HTML will generally become the input value used to create that same
fragment of HTML the next time your `view` function is called.

@docs checkbox, radioButton, lineEdit, comboBox, slider


## Advanced

@docs custom

-}

import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import String


{-| Create a `<input type="checkbox">` element with the given attributes and
current value, and which produces `Bool` messages with the new value whenever
the checkbox is clicked.

See the [Embedding.elm](https://github.com/kintail/input-widget/blob/1.0.6/examples/Embedding.elm)
example for sample usage.

-}
checkbox : List (Html.Attribute Bool) -> Bool -> Html Bool
checkbox attributes value =
    Html.input
        (Attributes.type_ "checkbox"
            :: Attributes.checked value
            :: Events.onCheck identity
            :: attributes
        )
        []


{-| Create a `<input type="radio">` element with the given attributes. When the
radio button is checked, it will send a message equal to the first given value;
it will be displayed as currently checked if the two given values are equal to
each other.

To create a set of mutually-exclusive radio buttons (the usual case), call this
function multiple times, passing a different first value each time (the value to
be selected if that radio button is clicked) but the same second value (the
currently selected value). This way, only the radio button whose value matches
the currently selected value will be displayed as checked. When any other radio
button is clicked, it will emit a message equal to its specific value, so that
you can update the currently selected value to that value.

See the [RadioButton.elm](https://github.com/kintail/input-widget/blob/1.0.6/examples/RadioButton.elm)
example for sample usage.

-}
radioButton : List (Html.Attribute a) -> a -> a -> Html a
radioButton attributes value currentValue =
    Html.input
        (Attributes.type_ "radio"
            :: Attributes.checked (value == currentValue)
            :: Events.onCheck (always value)
            :: attributes
        )
        []


{-| Create a simple `<input>` element with the given attributes and text to
display. A message will be sent with the updated text whenever the text is
edited.

See the [Validation.elm](https://github.com/kintail/input-widget/blob/1.0.6/examples/Validation.elm)
example for sample usage.

-}
lineEdit : List (Html.Attribute String) -> String -> Html String
lineEdit attributes value =
    Html.input
        (Attributes.value value
            :: Events.onInput identity
            :: attributes
        )
        []


{-| Create a `<select>` element with the given attributes. The `<select>`
element will be populated by `<option>` elements defined by the given list of
values, converted to text using the given function. The final given value is the
one that should be displayed as selected. A message will be sent with the newly
selected value whenever the selection is changed, either via keyboard or click.

Note that the currently selected value should be one of the values in the list,
and the list should not contain any duplicates. Otherwise it is possible that
either no values or more than one value will be marked as `selected` in the
resulting HTML.

See the [ComboBox.elm](https://github.com/kintail/input-widget/blob/1.0.6/examples/ComboBox.elm)
example for sample usage.

-}
comboBox : List (Html.Attribute a) -> (a -> String) -> List a -> a -> Html a
comboBox attributes toStr allItems =
    let
        itemsArray =
            Array.fromList allItems

        selectedIndexDecoder =
            Decode.at [ "target", "selectedIndex" ] Decode.int

        newSelectionDecoder currentItem =
            selectedIndexDecoder
                |> Decode.andThen
                    (\selectedIndex ->
                        case Array.get selectedIndex itemsArray of
                            Just newItem ->
                                if newItem /= currentItem then
                                    Decode.succeed newItem

                                else
                                    Decode.fail "selected item did not change"

                            Nothing ->
                                Decode.fail "selected index out of range"
                    )
    in
    \currentItem ->
        let
            decoder =
                newSelectionDecoder currentItem

            onChange =
                Events.on "change" decoder

            onKeyUp =
                Events.on "keyup" decoder

            toOption item =
                Html.option [ Attributes.selected (item == currentItem) ]
                    [ Html.text (toStr item) ]
        in
        Html.select (onChange :: onKeyUp :: attributes)
            (List.map toOption allItems)


{-| Create a `<range>` element with the given attributes, bounds, step size and
current value. A message will be sent with the updated value whenever the slider
is dragged.

See the [Slider.elm](https://github.com/kintail/input-widget/blob/1.0.6/examples/Slider.elm)
example for sample usage.

-}
slider :
    List (Html.Attribute Float)
    -> { min : Float, max : Float, step : Float }
    -> Float
    -> Html Float
slider attributes { min, max, step } =
    let
        targetValueDecoder currentValue =
            Decode.map (String.toFloat >> Maybe.withDefault currentValue)
                Events.targetValue

        newValueDecoder currentValue =
            targetValueDecoder currentValue
                |> Decode.andThen
                    (\newValue ->
                        if newValue /= currentValue then
                            Decode.succeed newValue

                        else
                            Decode.fail "value did not change"
                    )

        commonAttributes =
            Attributes.property "min" (Encode.float min)
                :: Attributes.property "max" (Encode.float max)
                :: Attributes.property "step" (Encode.float step)
                :: attributes
    in
    \currentValue ->
        Html.input
            (Attributes.type_ "range"
                :: Attributes.property "value" (Encode.float currentValue)
                :: Events.on "input" (newValueDecoder currentValue)
                :: Events.on "change" (newValueDecoder currentValue)
                :: commonAttributes
            )
            []


{-| Create a custom input widget from `view` and `update` functions of the same
form as used in standard Elm Architecture programs.

The `view` function should accept as input the current value to display, and
produce a fragment of HTML displaying that value that produces messages of some
arbitrary type of your choice. The `update` function should accept a message of
that type and the current value, and return an updated value. When called as

    InputWidget.custom { view = view, update = update }

this function will then return a function in the standard form `a -> Html a`.
Note that regardless of the message type used internally by `view` and `update`,
the only messages produced by the returned view function will be 'new value'
messages of type `a`.

See the [Custom.elm](https://github.com/kintail/input-widget/blob/1.0.6/examples/Custom.elm)
example for sample usage.

-}
custom : { view : a -> Html msg, update : msg -> a -> a } -> a -> Html a
custom { view, update } =
    \value -> view value |> Html.map (\message -> update message value)
