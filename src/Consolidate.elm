module Consolidate exposing (..)

import Process
import Question exposing (AnswerPlace, GivenAnswer(..), PlayingQuestion, Question, QuestionClass, newPlayingQuestion, removeSelection)
import Random
import Random.List
import Task


type alias State =
    { comingQuestions : ComingQuestions
    , activeQClasses : List QuestionClass
    , allQs : List Question
    , currentQuestion : Maybe PlayingQuestion
    , numLeft : Int
    }


type ComingQuestions
    = ReminderQs (List ( Question, QuestionClass ))
    | AllQs (List ( Question, QuestionClass )) (List Question)


questionsLeft : State -> Int
questionsLeft state =
    case state.comingQuestions of
        ReminderQs xs ->
            List.length xs

        AllQs xs ys ->
            List.length xs + List.length ys



-- Update


type Msg
    = GaveAnswerPlace AnswerPlace
    | GotShuffledRetests (List Question)
    | NeedNewQuestion
    | RepeatQuestion
    | GotNewQuestion (Maybe ( PlayingQuestion, List Question ))


update : Msg -> State -> ( Maybe State, Cmd Msg )
update msg state =
    case state of
        conState ->
            case msg of
                GaveAnswerPlace place ->
                    case conState.currentQuestion of
                        Nothing ->
                            ( Just state, Cmd.none )

                        Just pq ->
                            let
                                newState =
                                    { state
                                        | currentQuestion =
                                            Just { pq | selectedPlace = Just <| Place place }
                                    }
                            in
                            if place == pq.correctPlace then
                                ( Just newState
                                , Process.sleep 500 |> Task.perform (always NeedNewQuestion)
                                )

                            else
                                ( Just newState, Process.sleep 500 |> Task.perform (always RepeatQuestion) )

                GotShuffledRetests retests ->
                    case conState.comingQuestions of
                        ReminderQs qs ->
                            getNewQuestion { conState | comingQuestions = AllQs qs retests }

                        AllQs _ _ ->
                            ( Just state, Cmd.none )

                NeedNewQuestion ->
                    getNewQuestion conState

                RepeatQuestion ->
                    case conState.currentQuestion of
                        Nothing ->
                            ( Just state, Cmd.none )

                        Just current ->
                            ( Just <| { conState | currentQuestion = Just <| removeSelection current }
                            , Cmd.none
                            )

                GotNewQuestion Nothing ->
                    ( Nothing
                    , Cmd.none
                    )

                GotNewQuestion (Just ( playingQ, _ )) ->
                    ( Just
                        { conState
                            | currentQuestion = Just playingQ
                            , numLeft = questionsLeft state
                        }
                    , Cmd.none
                    )


getNewQuestion : State -> ( Maybe State, Cmd Msg )
getNewQuestion cState =
    case cState.comingQuestions of
        ReminderQs _ ->
            ( Nothing, Cmd.none )

        AllQs [] [] ->
            ( Nothing, Cmd.none )

        AllQs [] (q :: qs) ->
            ( Just <| { cState | comingQuestions = AllQs [] qs }
            , Random.generate GotNewQuestion <| newPlayingQuestion [ q ] cState.allQs cState.activeQClasses
            )

        AllQs (( q, qClass ) :: qqs) qs ->
            ( Just <| { cState | comingQuestions = AllQs qqs qs }
            , Random.generate GotNewQuestion <| newPlayingQuestion [ q ] cState.allQs [ qClass ]
            )


setupRetests : List Question -> Cmd Msg
setupRetests questions =
    Random.generate GotShuffledRetests <|
        Random.List.shuffle <|
            List.concatMap (\q -> List.repeat 5 q) questions


setupReminders : List Question -> List QuestionClass -> List ( Question, QuestionClass )
setupReminders questions qClasses =
    List.concatMap (\q -> List.map (\qClass -> ( q, qClass )) qClasses) questions
