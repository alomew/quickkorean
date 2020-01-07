module Numbers exposing (Msg, State, initStateCmd, update, view)

import Browser
import Dict exposing (Dict)
import Element exposing (centerX, centerY, el, focused, height, padding, shrink, spacing, width)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Json.Decode as Decode
import List.Extra as List
import Random exposing (Generator)
import Random.Extra as Random


main : Program () State Msg
main =
    Browser.element
        { init = \_ -> initStateCmd
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- model


type alias State =
    { currentNumber : Maybe ( Int, String )
    , currentInput : String
    , doneBad : Bool
    }


initState : State
initState =
    { currentNumber = Nothing
    , currentInput = ""
    , doneBad = False
    }


initStateCmd : ( State, Cmd Msg )
initStateCmd =
    ( initState
    , Random.generate GendNewNumber <| lowBiasedInt
    )



-- Utils


sinoDigits : Dict Int String
sinoDigits =
    Dict.fromList
        [ ( 1, "일" )
        , ( 2, "이" )
        , ( 3, "삼" )
        , ( 4, "사" )
        , ( 5, "오" )
        , ( 6, "육" )
        , ( 7, "칠" )
        , ( 8, "팔" )
        , ( 9, "구" )
        ]


sino =
    { ten = "십"
    , hundred = "백"
    , thousand = "천"
    , tenThousand = "만"
    }


getSinoDigitNonUnit : Int -> String
getSinoDigitNonUnit i =
    if i == 1 then
        ""

    else
        Dict.get i sinoDigits |> Maybe.withDefault ""


ascendingDigits : Int -> ( Int, List Int )
ascendingDigits num =
    let
        helper i =
            if i == 0 then
                []

            else
                modBy 10 i :: helper (i // 10)
    in
    ( modBy 10 num, helper (num // 10) )


inSinoKorean : Int -> String
inSinoKorean num =
    if num <= 0 then
        ""

    else
        let
            ( onesPlace, otherPlaces ) =
                ascendingDigits num

            bigDigitsWithUnit =
                List.zip
                    otherPlaces
                    [ sino.ten, sino.hundred, sino.thousand, sino.tenThousand ]
        in
        ((Dict.get onesPlace sinoDigits |> Maybe.withDefault "")
            :: List.map
                (\( dig, unit ) ->
                    getSinoDigitNonUnit dig
                        ++ (if dig /= 0 then
                                unit

                            else
                                ""
                           )
                )
                bigDigitsWithUnit
        )
            |> List.reverse
            |> String.join ""



-- Update


type Msg
    = GendNewNumber Int
    | UpdatedInput String
    | SubmittedAnswer
    | Start


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        Start ->
            initStateCmd

        GendNewNumber n ->
            ( { state | currentNumber = Just ( n, inSinoKorean n ) }
            , Cmd.none
            )

        UpdatedInput s ->
            ( { state | currentInput = s }
            , Cmd.none
            )

        SubmittedAnswer ->
            case state.currentNumber of
                Just ( n, _ ) ->
                    case String.toInt state.currentInput of
                        Just ans ->
                            if ans == n then
                                initStateCmd

                            else
                                ( { state | doneBad = True }
                                , Cmd.none
                                )

                        Nothing ->
                            ( { state | doneBad = True }
                            , Cmd.none
                            )

                Nothing ->
                    ( state, Cmd.none )



-- Random


lowBiasedInt : Generator Int
lowBiasedInt =
    Random.frequency
        ( 10, Random.int 1 9 )
        [ ( 20, Random.int 10 99 )
        , ( 10, Random.int 100 999 )
        , ( 5, Random.int 1000 9999 )
        , ( 2, Random.int 10000 99999 )
        ]



-- view


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


view : State -> Html Msg
view state =
    Element.layout [] <|
        case state.currentNumber of
            Just ( _, nInSino ) ->
                Element.column
                    [ height shrink
                    , centerX
                    , width shrink
                    , spacing 40
                    , padding 30
                    , Font.size 70
                    ]
                    [ el [ centerX, padding 100 ] (Element.text nInSino)
                    , Input.text
                        [ Element.htmlAttribute <| Attr.autofocus True
                        , onEnter SubmittedAnswer
                        , Font.size 50
                        ]
                        { onChange = \s -> UpdatedInput s
                        , text = state.currentInput
                        , placeholder = Nothing
                        , label = Input.labelHidden "Answer"
                        }
                    ]

            Nothing ->
                Element.none
