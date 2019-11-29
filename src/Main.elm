module Main exposing (..)

import Browser
import Browser.Events
import Element exposing (Element, alignLeft, alignRight, alignTop, centerX, centerY, column, el, fill, height, padding, paddingEach, pointer, rgb, row, shrink, spacing, text, width)
import Element.Events exposing (onClick)
import Element.Font exposing (color, size)
import Element.Input as Input exposing (button)
import Html exposing (Html)
import Json.Decode as Decode
import List.Extra
import Maybe exposing (Maybe)
import Process
import Question exposing (..)
import QuestionStore exposing (allQuestions)
import Random
import Set exposing (Set)
import Task



-- Main


main : Program () Game Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- Model


type Screen
    = QuestionSelect
    | Quiz


type alias Game =
    { score : Int
    , currentQuestion :
        Maybe PlayingQuestion
    , questions : QuestionsInWaiting
    , activeQClasses : List QuestionClass
    , activeCats : Set Category
    , screen : Screen
    , ready : Bool
    }


allCats : List Category
allCats =
    Set.toList <| allCategories allQuestions


type QuestionsInWaiting
    = Purgatory
    | Sifted (List Question)
    | Shuffled
        { left : List Question
        , all : List Question
        , retry : List Question
        , progress : ( Int, Int )
        }


readyToStart : List Question -> Bool
readyToStart qs =
    let
        everyQClassHappy =
            allQuestionClasses
                |> List.map
                    (\qClass ->
                        List.filter
                            (\question ->
                                List.member qClass (compatibleClasses question)
                            )
                            qs
                            |> List.length
                    )
                |> List.all (\count -> count == 0 || count >= 3)

        weHathAQuestion =
            List.length qs > 0
    in
    weHathAQuestion && everyQClassHappy


init : () -> ( Game, Cmd Msg )
init _ =
    ( { score = 0
      , currentQuestion = Nothing
      , questions = Purgatory
      , activeQClasses = []
      , activeCats = Set.empty
      , screen = QuestionSelect
      , ready = False
      }
    , Cmd.none
    )


siftQuestions : Game -> List Question
siftQuestions g =
    compatibleQuestions g.activeQClasses g.activeCats allQuestions


type QuestionOption
    = Category String
    | Direction QuestionClass



-- Update


type Msg
    = Answer GivenAnswer
    | NextQuestions (Maybe ( PlayingQuestion, List Question ))
    | Null
    | NeedNewQuestion
    | Start
    | ShuffledQs (List Question)
    | ToggledOption QuestionOption Bool


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        NextQuestions a ->
            case a of
                Just ( nextQuestion, remainingQuestions ) ->
                    ( { game
                        | currentQuestion = Just nextQuestion
                        , questions =
                            case game.questions of
                                Purgatory ->
                                    Purgatory

                                Sifted allQs ->
                                    Shuffled
                                        { left = remainingQuestions
                                        , all = allQs
                                        , retry = []
                                        , progress = ( 0, List.length remainingQuestions )
                                        }

                                Shuffled qs ->
                                    Shuffled
                                        { qs
                                            | left = remainingQuestions
                                            , progress =
                                                ( 1 + Tuple.first qs.progress
                                                , Tuple.second qs.progress
                                                )
                                        }
                      }
                    , Cmd.none
                    )

                Nothing ->
                    case game.questions of
                        Purgatory ->
                            ( game, Cmd.none )

                        _ ->
                            ( { game
                                | currentQuestion = Nothing
                              }
                            , Random.generate ShuffledQs <|
                                shuffleQuestions
                                    (case game.questions of
                                        Purgatory ->
                                            []

                                        Sifted allQs ->
                                            allQs

                                        Shuffled { retry, all } ->
                                            case retry of
                                                [] ->
                                                    all

                                                _ ->
                                                    retry
                                    )
                            )

        Answer answer ->
            giveAnswer answer game

        Null ->
            ( game, Cmd.none )

        NeedNewQuestion ->
            ( game
            , case game.questions of
                Shuffled { left, all } ->
                    Random.generate NextQuestions
                        (newPlayingQuestion
                            left
                            all
                            game.activeQClasses
                        )

                _ ->
                    Cmd.none
            )

        Start ->
            case game.questions of
                Purgatory ->
                    ( game, Cmd.none )

                Shuffled _ ->
                    ( game, Cmd.none )

                Sifted sifted ->
                    if readyToStart sifted then
                        ( { game | screen = Quiz }
                        , Random.generate ShuffledQs <| shuffleQuestions sifted
                        )

                    else
                        ( game, Cmd.none )

        ShuffledQs qs ->
            case game.questions of
                Purgatory ->
                    ( game, Cmd.none )

                Sifted all ->
                    ( { game
                        | questions = Shuffled { all = all, left = qs, retry = [], progress = ( 0, List.length qs ) }
                      }
                    , Random.generate NextQuestions (newPlayingQuestion qs all game.activeQClasses)
                    )

                Shuffled ({ all } as lar) ->
                    ( { game
                        | questions = Shuffled { lar | left = qs, retry = [], progress = ( 0, List.length qs ) }
                      }
                    , Random.generate NextQuestions (newPlayingQuestion qs all game.activeQClasses)
                    )

        ToggledOption option toInsert ->
            case option of
                Category cat ->
                    let
                        currentCats =
                            if toInsert then
                                Set.insert cat game.activeCats

                            else
                                Set.remove cat game.activeCats

                        questions =
                            compatibleQuestions
                                game.activeQClasses
                                currentCats
                                allQuestions
                    in
                    ( { game
                        | activeCats = currentCats
                        , questions =
                            Sifted <| questions
                        , ready = readyToStart questions
                      }
                    , Cmd.none
                    )

                Direction d ->
                    let
                        currentQClasses =
                            if toInsert then
                                d :: game.activeQClasses

                            else
                                List.Extra.remove d game.activeQClasses

                        questions =
                            compatibleQuestions
                                currentQClasses
                                game.activeCats
                                allQuestions
                    in
                    ( { game
                        | activeQClasses = currentQClasses
                        , questions =
                            Sifted <| questions
                        , ready = readyToStart questions
                      }
                    , Cmd.none
                    )


giveAnswer : GivenAnswer -> Game -> ( Game, Cmd Msg )
giveAnswer answer game =
    case answer of
        Place answerPlace ->
            giveAnswerPlace answerPlace game

        Unsure ->
            giveUnsure game


giveUnsure : Game -> ( Game, Cmd Msg )
giveUnsure game =
    case game.currentQuestion of
        Nothing ->
            ( game, Cmd.none )

        Just playingQuestion ->
            case playingQuestion.selectedPlace of
                Just _ ->
                    ( game, Cmd.none )

                Nothing ->
                    ( { game
                        | currentQuestion =
                            Just
                                { playingQuestion
                                    | selectedPlace = Just Unsure
                                }
                        , score = game.score - 1
                        , questions =
                            case game.questions of
                                Shuffled lar ->
                                    Shuffled { lar | retry = playingQuestion.question :: lar.retry }

                                _ ->
                                    game.questions
                      }
                    , Process.sleep 500 |> Task.perform (always NeedNewQuestion)
                    )


giveAnswerPlace : AnswerPlace -> Game -> ( Game, Cmd Msg )
giveAnswerPlace answerPlace game =
    case game.currentQuestion of
        Nothing ->
            ( game, Cmd.none )

        Just playingQuestion ->
            case playingQuestion.selectedPlace of
                Just _ ->
                    ( game, Cmd.none )

                Nothing ->
                    ( { game
                        | currentQuestion = Just { playingQuestion | selectedPlace = Just <| Place answerPlace }
                        , score =
                            game.score
                                + (if answerPlace == playingQuestion.correctPlace then
                                    1

                                   else
                                    -1
                                  )
                      }
                    , Process.sleep 500 |> Task.perform (always NeedNewQuestion)
                    )



-- Subscriptions


actionKeyDecoder : Decode.Decoder Msg
actionKeyDecoder =
    Decode.map actionFromKey (Decode.field "key" Decode.string)


actionFromKey : String -> Msg
actionFromKey keyString =
    let
        placeMap =
            [ ( [ "z", "1" ], Place LeftPlace )
            , ( [ "x", "2" ], Place MiddlePlace )
            , ( [ "c", "3" ], Place RightPlace )
            , ( [ "u", "0" ], Unsure )
            ]
    in
    case List.Extra.find (\( keys, _ ) -> List.member keyString keys) placeMap of
        Just ( _, place ) ->
            Answer place

        Nothing ->
            Null


subscriptions : Game -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown actionKeyDecoder



-- View


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


type AnswerMark
    = Selected
    | Unselected
    | UnsureMark


viewAnswers : ( String, String, String ) -> Maybe GivenAnswer -> Element Msg
viewAnswers ( leftOpt, midOpt, rightOpt ) selected =
    let
        optionText place =
            case place of
                LeftPlace ->
                    leftOpt

                MiddlePlace ->
                    midOpt

                RightPlace ->
                    rightOpt

        blue =
            rgb 0 0 1

        black =
            rgb 0 0 0

        yellow =
            rgb 1 1 0

        selectedPlace place =
            case selected of
                Nothing ->
                    Unselected

                Just Unsure ->
                    UnsureMark

                Just (Place sp) ->
                    if place == sp then
                        Selected

                    else
                        Unselected

        option place =
            el
                [ onClick (Answer <| Place place)
                , centerX
                , pointer
                , case selectedPlace place of
                    Selected ->
                        color blue

                    Unselected ->
                        color black

                    UnsureMark ->
                        color yellow
                ]
                (text <| optionText place)
    in
    column [ size 40, spacing 20, centerX ]
        [ option LeftPlace, option MiddlePlace, option RightPlace ]


view : Game -> Html Msg
view g =
    case g.screen of
        Quiz ->
            viewQuiz g

        QuestionSelect ->
            viewQuestionSelect g


viewSelector : Category -> (Bool -> Msg) -> Bool -> Element Msg
viewSelector label msg checked =
    Input.checkbox [ width shrink ]
        { onChange = msg
        , icon = Input.defaultCheckbox
        , checked = checked
        , label =
            Input.labelRight []
                (text label)
        }


textOfQClass : QuestionClass -> String
textOfQClass qClass =
    case qClass of
        Question.KorToEng ->
            "Korean to English"

        Question.EngToKor ->
            "English to Korean"

        Question.Pronounce ->
            "Pronunciation of Hangeul"


viewQuestionSelect : Game -> Html Msg
viewQuestionSelect game =
    Element.layout [ size 30 ] <|
        column [ width fill, spacing 50, height shrink, centerY ]
            [ row [ width fill, spacing 30 ]
                [ column [ alignLeft, spacing 10, width fill, alignTop ]
                    [ el [ centerX, width shrink ] (text "Topics")
                    , column [ centerX, width shrink ] <|
                        List.map
                            (\cat ->
                                viewSelector
                                    cat
                                    (ToggledOption (Category cat))
                                    (Set.member
                                        cat
                                        game.activeCats
                                    )
                            )
                            allCats
                    ]
                , column [ alignRight, spacing 10, width fill, alignTop ]
                    [ el [ centerX, width shrink ] (text "Question Styles")
                    , column [ centerX, width shrink ] <|
                        List.map
                            (\qClass ->
                                viewSelector
                                    (textOfQClass qClass)
                                    (ToggledOption (Direction qClass))
                                    (List.member
                                        qClass
                                        game.activeQClasses
                                    )
                            )
                            allQuestionClasses
                    ]
                ]
            , button
                [ centerX
                , size 40
                , color <|
                    if game.ready then
                        rgb 0 0 0

                    else
                        rgb 1 0 0
                ]
                { onPress =
                    if game.ready then
                        Just Start

                    else
                        Nothing
                , label = text "Start"
                }
            ]


viewQuiz : Game -> Html Msg
viewQuiz game =
    Element.layout [] <|
        column [ centerX, width shrink, padding 50, spacing 50, height fill ]
            [ case game.questions of
                Shuffled { progress } ->
                    el [ centerX ]
                        (text <|
                            "Question "
                                ++ String.fromInt (Tuple.first progress)
                                ++ " of "
                                ++ String.fromInt (Tuple.second progress)
                                ++ " this round."
                        )

                _ ->
                    Element.none
            , column
                [ spacing 5, centerX, height shrink ]
                [ el [ size 20, centerX ]
                    (text "Score")
                , el [ size 40, centerX ]
                    (text <| String.fromInt game.score)
                ]
            , column [ spacing 20, centerX ]
                (case game.currentQuestion of
                    Just cq ->
                        [ el
                            [ size 70
                            , centerX
                            , paddingEach { edges | bottom = 30 }
                            , height shrink
                            , case cq.selectedPlace of
                                Just (Place sp) ->
                                    if sp == cq.correctPlace then
                                        color (rgb 0 1 0)

                                    else
                                        color (rgb 1 0 0)

                                Just Unsure ->
                                    color (rgb 1 1 0)

                                Nothing ->
                                    color (rgb 0 0 0)
                            ]
                          <|
                            text cq.prompt
                        , viewAnswers cq.options cq.selectedPlace
                        , button [ centerX ] { onPress = Just (Answer Unsure), label = text "Unsure" }
                        ]

                    Nothing ->
                        [ Element.none ]
                )
            , button [ centerX ] { onPress = Just Start, label = text "Start" }
            ]
