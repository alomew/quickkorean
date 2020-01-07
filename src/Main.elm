module Main exposing (..)

import Browser
import Browser.Events
import Consolidate as Con
import Element exposing (Element, alignLeft, alignRight, alignTop, centerX, centerY, column, el, fill, height, padding, paddingEach, pointer, px, rgb, rgb255, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font exposing (color, size)
import Element.Input as Input exposing (button)
import Html exposing (Html)
import Json.Decode as Decode
import List.Extra
import Maybe exposing (Maybe)
import Numbers
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


type QuizState
    = FirstRound FirstRoundState
    | Consolidate Con.State


type alias FirstRoundState =
    { questions : QuestionsInWaiting
    , activeQClasses : List QuestionClass
    , activeCats : Set Category
    , currentQuestion :
        Maybe PlayingQuestion
    , score : Int
    , progress : Int
    }


type alias SelectionData =
    { activeQClasses : List QuestionClass
    , activeCats : Set Category
    , questions : List Question
    }


type Game
    = QuestionSelect SelectionData
    | Quiz QuizState
    | Numbers Numbers.State


allCats : List Category
allCats =
    Set.toList <| allCategories allQuestions


type QuestionsInWaiting
    = Sifted (List Question)
    | Shuffled
        { left : List Question
        , all : List Question
        , retry : List Question
        , number : Int
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


resettedGame : Game
resettedGame =
    QuestionSelect
        { activeQClasses = [ Question.KorToEng, Question.EngToKor ]
        , activeCats = Set.empty
        , questions = []
        }


init : () -> ( Game, Cmd Msg )
init _ =
    ( resettedGame
    , Cmd.none
    )


siftQuestions : List QuestionClass -> Set Category -> List Question
siftQuestions qClasses cats =
    compatibleQuestions qClasses cats allQuestions


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
    | Reset
    | NumberMsg Numbers.Msg
    | StartNumbers
    | ConsolidateMsg Con.Msg


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        Reset ->
            ( resettedGame, Cmd.none )

        NextQuestions a ->
            case game of
                Quiz (FirstRound quizState) ->
                    case a of
                        Just ( nextQuestion, remainingQuestions ) ->
                            ( Quiz <|
                                FirstRound
                                    { quizState
                                        | currentQuestion = Just nextQuestion
                                        , questions =
                                            case quizState.questions of
                                                Sifted allQs ->
                                                    Shuffled
                                                        { left = remainingQuestions
                                                        , all = allQs
                                                        , retry = []
                                                        , number = List.length remainingQuestions
                                                        }

                                                Shuffled qs ->
                                                    Shuffled
                                                        { qs
                                                            | left = remainingQuestions
                                                        }
                                    }
                            , Cmd.none
                            )

                        Nothing ->
                            case quizState.questions of
                                Sifted allQs ->
                                    ( Quiz <| FirstRound { quizState | currentQuestion = Nothing }
                                    , Random.generate ShuffledQs <|
                                        shuffleQuestions allQs
                                    )

                                Shuffled { retry, all } ->
                                    case retry of
                                        [] ->
                                            ( resettedGame
                                            , Cmd.none
                                            )

                                        _ ->
                                            ( Quiz <|
                                                Consolidate
                                                    { currentQuestion = Nothing
                                                    , comingQuestions = Con.ReminderQs <| Con.setupReminders retry quizState.activeQClasses
                                                    , allQs = all
                                                    , activeQClasses = quizState.activeQClasses
                                                    , numLeft = 0
                                                    }
                                            , Cmd.map ConsolidateMsg <| Con.setupRetests retry
                                            )

                _ ->
                    ( game, Cmd.none )

        Answer answer ->
            case game of
                Quiz (FirstRound quiz) ->
                    giveAnswer answer { quiz | progress = quiz.progress + 1 }

                _ ->
                    ( game, Cmd.none )

        Null ->
            ( game, Cmd.none )

        NeedNewQuestion ->
            case game of
                Quiz (FirstRound quiz) ->
                    ( game
                    , case quiz.questions of
                        Shuffled { left, all } ->
                            Random.generate NextQuestions
                                (newPlayingQuestion
                                    left
                                    all
                                    quiz.activeQClasses
                                )

                        _ ->
                            Cmd.none
                    )

                _ ->
                    ( game, Cmd.none )

        Start ->
            case game of
                QuestionSelect ({ questions } as selections) ->
                    if readyToStart questions then
                        ( Quiz <|
                            FirstRound
                                { questions = Sifted questions
                                , currentQuestion = Nothing
                                , score = 0
                                , progress = 0
                                , activeQClasses = selections.activeQClasses
                                , activeCats = selections.activeCats
                                }
                        , Random.generate ShuffledQs <| shuffleQuestions questions
                        )

                    else
                        ( game, Cmd.none )

                _ ->
                    ( game, Cmd.none )

        ShuffledQs qs ->
            case game of
                Quiz (FirstRound quizState) ->
                    case quizState.questions of
                        Sifted all ->
                            ( Quiz <|
                                FirstRound
                                    { quizState
                                        | questions = Shuffled { all = all, left = qs, retry = [], number = List.length qs }
                                    }
                            , Random.generate NextQuestions (newPlayingQuestion qs all quizState.activeQClasses)
                            )

                        Shuffled ({ all } as lar) ->
                            ( Quiz <|
                                FirstRound
                                    { quizState
                                        | questions =
                                            Shuffled
                                                { lar
                                                    | left = qs
                                                    , retry = []
                                                    , number = List.length qs
                                                }
                                        , score = 0
                                        , progress = 0
                                    }
                            , Random.generate NextQuestions (newPlayingQuestion qs all quizState.activeQClasses)
                            )

                _ ->
                    ( game, Cmd.none )

        ToggledOption option toInsert ->
            case game of
                QuestionSelect selections ->
                    case option of
                        Category cat ->
                            let
                                currentCats =
                                    if toInsert then
                                        Set.insert cat selections.activeCats

                                    else
                                        Set.remove cat selections.activeCats

                                questions =
                                    compatibleQuestions
                                        selections.activeQClasses
                                        currentCats
                                        allQuestions
                            in
                            ( QuestionSelect
                                { selections
                                    | activeCats = currentCats
                                    , questions =
                                        questions
                                }
                            , Cmd.none
                            )

                        Direction d ->
                            let
                                currentQClasses =
                                    if toInsert then
                                        d :: selections.activeQClasses

                                    else
                                        List.Extra.remove d selections.activeQClasses

                                questions =
                                    compatibleQuestions
                                        currentQClasses
                                        selections.activeCats
                                        allQuestions
                            in
                            ( QuestionSelect
                                { selections
                                    | activeQClasses = currentQClasses
                                    , questions =
                                        questions
                                }
                            , Cmd.none
                            )

                _ ->
                    ( game, Cmd.none )

        NumberMsg nMsg ->
            case game of
                Numbers nState ->
                    let
                        ( newNState, newNCmd ) =
                            Numbers.update nMsg nState
                    in
                    ( Numbers newNState
                    , Cmd.map NumberMsg newNCmd
                    )

                _ ->
                    ( game
                    , Cmd.none
                    )

        ConsolidateMsg cMsg ->
            case game of
                Quiz (Consolidate state) ->
                    let
                        ( maybeNewCState, newCCmd ) =
                            Con.update cMsg state
                    in
                    case maybeNewCState of
                        Nothing ->
                            ( resettedGame, Cmd.none )

                        Just newCState ->
                            ( Quiz <| Consolidate newCState
                            , Cmd.map ConsolidateMsg newCCmd
                            )

                _ ->
                    ( game, Cmd.none )

        StartNumbers ->
            let
                ( nState, nCmd ) =
                    Numbers.initStateCmd
            in
            ( Numbers nState
            , Cmd.map NumberMsg nCmd
            )


giveAnswer : GivenAnswer -> FirstRoundState -> ( Game, Cmd Msg )
giveAnswer answer quiz =
    case answer of
        Place answerPlace ->
            giveAnswerPlace answerPlace quiz

        Unsure ->
            giveUnsure quiz


giveUnsure : FirstRoundState -> ( Game, Cmd Msg )
giveUnsure quiz =
    case quiz.currentQuestion of
        Nothing ->
            ( Quiz <| FirstRound quiz, Cmd.none )

        Just playingQuestion ->
            case playingQuestion.selectedPlace of
                Just _ ->
                    ( Quiz <| FirstRound quiz, Cmd.none )

                Nothing ->
                    ( Quiz <|
                        FirstRound
                            { quiz
                                | currentQuestion =
                                    Just
                                        { playingQuestion
                                            | selectedPlace = Just Unsure
                                        }
                                , questions =
                                    case quiz.questions of
                                        Shuffled lar ->
                                            Shuffled { lar | retry = playingQuestion.question :: lar.retry }

                                        _ ->
                                            quiz.questions
                            }
                    , Process.sleep 500 |> Task.perform (always NeedNewQuestion)
                    )


giveAnswerPlace : AnswerPlace -> FirstRoundState -> ( Game, Cmd Msg )
giveAnswerPlace answerPlace quiz =
    case quiz.currentQuestion of
        Nothing ->
            ( Quiz <| FirstRound quiz, Cmd.none )

        Just playingQuestion ->
            case playingQuestion.selectedPlace of
                Just _ ->
                    ( Quiz <| FirstRound quiz, Cmd.none )

                Nothing ->
                    ( Quiz <|
                        FirstRound
                            ({ quiz
                                | currentQuestion = Just { playingQuestion | selectedPlace = Just <| Place answerPlace }
                             }
                                |> (\q ->
                                        if answerPlace == playingQuestion.correctPlace then
                                            { q
                                                | score = q.score + 1
                                            }

                                        else
                                            { q
                                                | questions =
                                                    case quiz.questions of
                                                        Shuffled lar ->
                                                            Shuffled { lar | retry = playingQuestion.question :: lar.retry }

                                                        _ ->
                                                            quiz.questions
                                            }
                                   )
                            )
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
            if keyString == "q" then
                Reset

            else
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


viewAnswers : ( String, String, String ) -> Maybe GivenAnswer -> Bool -> Element Msg
viewAnswers ( leftOpt, midOpt, rightOpt ) selected isFirstRound =
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
                [ onClick <|
                    if isFirstRound then
                        Answer <| Place place

                    else
                        ConsolidateMsg (Con.GaveAnswerPlace place)
                , centerX
                , pointer
                , case selectedPlace place of
                    Selected ->
                        color blue

                    Unselected ->
                        color black

                    UnsureMark ->
                        color yellow
                , Border.rounded 6
                , Element.mouseOver [ Background.color hoverGrey ]
                , padding 10
                ]
                (text <| optionText place)
    in
    column [ size 40, spacing 5, centerX ]
        [ option LeftPlace, option MiddlePlace, option RightPlace ]


view : Game -> Html Msg
view game =
    case game of
        Quiz (FirstRound quizState) ->
            viewQuiz quizState

        Quiz (Consolidate state) ->
            viewConsolidate state

        QuestionSelect selections ->
            viewQuestionSelect selections

        Numbers nState ->
            Html.map NumberMsg <| Numbers.view nState


hoverGrey =
    rgb255 214 214 214


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


viewQuestionSelect : SelectionData -> Html Msg
viewQuestionSelect selections =
    Element.layout [ size 30 ] <|
        column [ width fill, height shrink, centerY, centerX, spacing 10 ]
            [ row [ width fill, spacing 20 ]
                [ el [ width fill ] Element.none
                , column [ alignLeft, spacing 20, width fill, alignTop ]
                    [ el [ centerX, width shrink, Font.bold ] (text "Topics")
                    , column [ centerX, width shrink, spacing 10 ] <|
                        List.map
                            (\cat ->
                                viewSelector
                                    cat
                                    (ToggledOption (Category cat))
                                    (Set.member
                                        cat
                                        selections.activeCats
                                    )
                            )
                            allCats
                    ]
                , column [ alignRight, spacing 20, width fill, alignTop ]
                    [ el [ centerX, width shrink, Font.bold ] (text "Question Styles")
                    , column [ centerX, width shrink, spacing 10 ] <|
                        List.map
                            (\qClass ->
                                viewSelector
                                    (textOfQClass qClass)
                                    (ToggledOption (Direction qClass))
                                    (List.member
                                        qClass
                                        selections.activeQClasses
                                    )
                            )
                            allQuestionClasses
                    ]
                , el [ width fill ] Element.none
                ]
            , el [ height (20 |> px) ] Element.none
            , el [ height (40 |> px), centerX, padding 10 ] <|
                let
                    readyProps =
                        { textColor = color <| rgb 0 0 0
                        , bgColor = rgb255 90 190 90
                        , onHover = [ Background.color hoverGrey ]
                        , onPress = Just Start
                        }

                    notReadyProps =
                        { textColor = color <| rgb 1 1 1
                        , bgColor = rgb255 255 76 48
                        , onHover = []
                        , onPress = Nothing
                        }

                    props =
                        if readyToStart selections.questions then
                            readyProps

                        else
                            notReadyProps
                in
                button
                    [ size 40
                    , props.textColor
                    , Background.color props.bgColor
                    , Element.mouseOver props.onHover
                    , Border.rounded 4
                    , padding 5
                    ]
                    { onPress = props.onPress
                    , label = text "Quiz"
                    }
            , el [ centerX, padding 30 ] <|
                button
                    [ size 40
                    , color <|
                        rgb 0 0 0
                    , Background.color (rgb255 90 190 90)
                    , Element.mouseOver [ Background.color hoverGrey ]
                    , Border.rounded 4
                    , padding 5
                    ]
                    { onPress = Just <| StartNumbers
                    , label = text "Numbers"
                    }
            ]


viewQuiz : FirstRoundState -> Html Msg
viewQuiz game =
    Element.layout [] <|
        column [ centerX, width shrink, padding 50, spacing 50, height fill ] <|
            [ case game.questions of
                Shuffled { number } ->
                    column [ centerX, size 30, spacing 5 ]
                        [ el [ centerX ]
                            (text <|
                                String.fromInt game.score
                                    ++ "/"
                                    ++ String.fromInt game.progress
                                    ++ " so far this round"
                            )
                        , el [ centerX, size 20 ] <|
                            text
                                (String.fromInt (number - game.progress)
                                    ++ " left"
                                )
                        ]

                _ ->
                    Element.none
            ]
                ++ viewPlayingQuestion game.currentQuestion True


viewConsolidate : Con.State -> Html Msg
viewConsolidate state =
    Element.layout [] <|
        column [ centerX, width shrink, padding 50, spacing 50, height fill ] <|
            [ el [ centerX, size 30 ] <|
                text "The Ones You Didn't Know"
            , el [ centerX, size 20 ] <|
                text
                    (String.fromInt state.numLeft
                        ++ " left"
                    )
            ]
                ++ viewPlayingQuestion state.currentQuestion False


viewPlayingQuestion : Maybe PlayingQuestion -> Bool -> List (Element Msg)
viewPlayingQuestion playingQuestion isFirstRound =
    [ column [ spacing 20, centerX ]
        (case playingQuestion of
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
                , viewAnswers cq.options cq.selectedPlace isFirstRound
                , if isFirstRound then
                    button [ Border.rounded 4, centerX, padding 5, Element.mouseOver [ Background.color hoverGrey ] ]
                        { onPress = Just (Answer Unsure), label = text "Unsure" }

                  else
                    Element.none
                ]

            Nothing ->
                [ Element.none ]
        )
    , el [ centerX ] <| text "Press q to return to menu."
    ]
