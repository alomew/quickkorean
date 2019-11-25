module Main exposing (..)

import Browser
import Browser.Events
import Element exposing (Element, centerX, column, el, fill, height, padding, paddingEach, pointer, rgb, shrink, spacing, text, width)
import Element.Events exposing (onClick)
import Element.Font exposing (color, size)
import Element.Input exposing (button)
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


type alias WordPair =
    { hang : String
    , phonetic : String
    }


type alias Game =
    { score : Int
    , currentQuestion :
        Maybe PlayingQuestion
    , questions : QuestionsInWaiting
    , activeQClasses : List QuestionClass
    , activeCats : Set Category
    }


type QuestionsInWaiting
    = Purgatory
    | Sifted (List Question)
    | Shuffled
        { left : List Question
        , all : List Question
        , retry : List Question
        , progress : ( Int, Int )
        }


init : () -> ( Game, Cmd Msg )
init _ =
    ( { score = 0
      , currentQuestion = Nothing
      , questions = Purgatory
      , activeQClasses = [ Question.EngToKor, Question.KorToEng ]
      , activeCats = Set.fromList [ "School" ]
      }
    , Cmd.none
    )


siftQuestions : Game -> List Question
siftQuestions g =
    compatibleQuestions g.activeQClasses g.activeCats allQuestions



-- Update


type Msg
    = Answer GivenAnswer
    | NextQuestions (Maybe ( PlayingQuestion, List Question ))
    | Null
    | NeedNewQuestion
    | Start
    | ShuffledQs (List Question)


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
            let
                sifted =
                    siftQuestions game
            in
            ( { game | questions = Sifted sifted }
            , Random.generate ShuffledQs <| shuffleQuestions sifted
            )

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
view game =
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
            , column [ spacing 20 ]
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
