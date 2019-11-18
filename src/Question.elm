module Question exposing (..)

import List
import List.Extra as ListE
import Random exposing (Generator)
import Random.List as RList
import Set exposing (Set)


type alias Question =
    { korean : String
    , answers : AnswerClass
    , categories : Set Category
    }


type AnswerClass
    = JustEnglish String
    | JustPronunciation String
    | EnglishAndPronunciation String String


type QuestionClass
    = KorToEng
    | EngToKor
    | Pronounce


type alias Category =
    String



-- we give the question classes that are compatible with a given question


compatibleClasses : Question -> List QuestionClass
compatibleClasses question =
    let
        ( first, rest ) =
            compatibleClassesSplit question
    in
    first :: rest


compatibleClassesSplit question =
    case question.answers of
        JustEnglish _ ->
            ( KorToEng, [ EngToKor ] )

        JustPronunciation _ ->
            ( Pronounce, [] )

        _ ->
            ( KorToEng, [ EngToKor, Pronounce ] )


compatibleCategories : Question -> Set Category -> Bool
compatibleCategories q cats =
    not <| Set.isEmpty <| Set.intersect q.categories cats


{-| Given the current `QuestionClass`es and `Category`ies, produce a generator
for a shuffled list of all available questions.
-}
compatibleQuestions : List QuestionClass -> Set Category -> List Question -> List Question
compatibleQuestions classes cats allQuestions =
    List.filter
        (\q ->
            (List.any (\class -> List.member class classes) <| compatibleClasses q)
                && compatibleCategories q cats
        )
        allQuestions


shuffleQuestions : List Question -> Generator (List Question)
shuffleQuestions =
    RList.shuffle



-- Random.Extra has the convenience function "choose" which gives us a maybe


{-| when given a `questionClass`, give a possible question of an appropriate category
-}
newQuestionOfClass : List Question -> QuestionClass -> Set Category -> Generator (Maybe ( Question, List Question ))
newQuestionOfClass possibleQuestions qClass cats =
    RList.choose (List.filter (\q -> (List.member qClass <| compatibleClasses q) && compatibleCategories q cats) possibleQuestions)
        |> Random.map
            (\( maybeQ, _ ) ->
                case maybeQ of
                    Just q ->
                        Just ( q, ListE.remove q possibleQuestions )

                    Nothing ->
                        Nothing
            )


{-| When given a number of `questionClass`es, give a
possible question if possible, or return `Nothing`
-}
newQuestion : List Question -> Maybe ( Question, List Question )
newQuestion qs =
    case qs of
        [] ->
            Nothing

        q :: qss ->
            Just ( q, qss )


extractAnswer : Question -> QuestionClass -> String
extractAnswer q qClass =
    case ( qClass, q.answers ) of
        ( EngToKor, _ ) ->
            q.korean

        ( KorToEng, JustEnglish s ) ->
            s

        ( _, JustEnglish _ ) ->
            q.korean

        ( _, JustPronunciation s ) ->
            s

        ( KorToEng, EnglishAndPronunciation e _ ) ->
            e

        ( Pronounce, EnglishAndPronunciation _ p ) ->
            p


extractPrompt : Question -> QuestionClass -> String
extractPrompt q qClass =
    case qClass of
        KorToEng ->
            q.korean

        Pronounce ->
            q.korean

        EngToKor ->
            case q.answers of
                JustEnglish e ->
                    e

                EnglishAndPronunciation e _ ->
                    e

                _ ->
                    "Oof something has gone horribly wrong"


{-| Given a load of questions and a questionClass, generates two fake answers to
the question.
-}
getWrongAnswers : List Question -> Question -> QuestionClass -> Generator ( String, String )
getWrongAnswers allQs currentQuestion qClass =
    RList.choose (List.filter (\q -> List.member qClass <| compatibleClasses q) (ListE.remove currentQuestion allQs))
        |> Random.andThen
            (\( maybeQ1, restQ1s ) ->
                case maybeQ1 of
                    Nothing ->
                        Random.constant ( "", "" )

                    Just q1 ->
                        RList.choose restQ1s
                            |> Random.andThen
                                (\( maybeQ2, _ ) ->
                                    case maybeQ2 of
                                        Nothing ->
                                            Random.constant ( extractAnswer q1 qClass, "" )

                                        Just q2 ->
                                            Random.constant ( extractAnswer q1 qClass, extractAnswer q2 qClass )
                                )
            )



-- Playing questions
{-
   The code below concerns playingQuestions, which are
   a torn-apart question
   that we assess and change dynamically in the app.
-}


type alias PlayingQuestion =
    { selectedPlace : Maybe AnswerPlace
    , options : ( String, String, String )
    , correctPlace : AnswerPlace
    , prompt : String
    , questionClass : QuestionClass
    }


type AnswerPlace
    = LeftPlace
    | MiddlePlace
    | RightPlace


insertAnswerIntoPlace answer option1 option2 correctPlace =
    case correctPlace of
        LeftPlace ->
            ( answer, option1, option2 )

        MiddlePlace ->
            ( option1, answer, option2 )

        RightPlace ->
            ( option1, option2, answer )


newPlayingQuestion : List Question -> List Question -> Random.Generator (Maybe ( PlayingQuestion, List Question ))
newPlayingQuestion availQs allQs =
    case availQs of
        [] ->
            Random.constant Nothing

        q :: restQs ->
            chooseQClass q
                |> Random.andThen
                    (\qClass ->
                        Random.map2
                            (\( a1, a2 ) correctPlace ->
                                Just
                                    ( PlayingQuestion
                                        Nothing
                                        (insertAnswerIntoPlace (extractAnswer q qClass) a1 a2 correctPlace)
                                        correctPlace
                                        (extractPrompt q qClass)
                                        qClass
                                    , restQs
                                    )
                            )
                            (getWrongAnswers allQs q qClass)
                            (Random.uniform LeftPlace [ MiddlePlace, RightPlace ])
                    )


chooseQClass : Question -> Generator QuestionClass
chooseQClass question =
    let
        ( one, others ) =
            compatibleClassesSplit question
    in
    Random.uniform one others
