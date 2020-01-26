module QuestionStore exposing (allQuestions)

import Question exposing (AnswerClass(..), Question)
import Set exposing (Set)


allQuestions : List Question
allQuestions =
    List.concat
        [ randomQuestions
        , locationQuestions
        , verbQuestions
        , objectQuestions
        , schoolQuestions
        , timeQuestions
        , greetingQuestions
        , instructionQuestions
        , jobQuestions
        , natNumQuestions
        , idCounterQuestions
        , accCounterQuestions
        , familyQuestions
        , lesson4class2
        , lesson5class1
        , lesson5class2
        ]


randomQuestions =
    List.map (\( hanguel, phonetic ) -> Question hanguel (JustPronunciation phonetic) (Set.fromList [ "Random" ]))
        [ ( "가다", "ka-ta" )
        , ( "개", "kae" )
        , ( "공부", "kong-bu" )
        , ( "과일", "kwa-il" )
        , ( "학교", "hak-kyo" )
        , ( "나", "na" )
        , ( "농구", "nong-ku" )
        , ( "저녁", "cheo-nyeo(k)" )
        , ( "운동", "un-tong" )
        , ( "두부", "tu-bu" )
        , ( "돈", "ton" )
        , ( "돼지", "twae-chi" )
        , ( "걷다", "kot-ta" )
        , ( "라면", "ra-myeon" )
        , ( "런던", "reon-teon" )
        , ( "둘", "tul" )
        , ( "달력", "tal-lyeo(k)" )
        , ( "모자", "mo-cha" )
        , ( "물", "mul" )
        , ( "뭐", "mweo" )
        , ( "곰", "kom" )
        , ( "바다", "ba-ta" )
        , ( "불", "bul" )
        , ( "별", "byeol" )
        , ( "덥다", "top-ta" )
        , ( "사과", "sa-gwa" )
        , ( "신발", "sin-bal" )
        , ( "쇠", "swae" )
        , ( "맛", "ma(t)" )
        , ( "아기", "a-gi" )
        , ( "언제", "eon-chae" )
        , ( "약", "ya(k)" )
        , ( "공", "kong" )
        , ( "주말", "chu-mal" )
        , ( "중국", "chung-kuk" )
        , ( "쥐", "chwi" )
        , ( "낮", "na(t)" )
        , ( "차", "tcha" )
        , ( "춤", "tchum" )
        , ( "최고", "chwae-go" )
        , ( "몇", "myeo(t)" )
        , ( "컵", "keop" )
        , ( "코", "ko" )
        , ( "퀴즈", "kwi-cheu" )
        , ( "부엌", "bu-eok" )
        , ( "토요일", "toh-yo-il" )
        , ( "털", "teohl" )
        , ( "튀김", "twhi-gim" )
        , ( "밭", "ba(t)" )
        , ( "피", "pi" )
        , ( "팔", "pal" )
        , ( "편지", "peyon-chi" )
        , ( "앞", "ap" )
        , ( "호주", "ho-chu" )
        , ( "한국", "han-gu(k)" )
        , ( "형", "hyeong" )
        , ( "좋다", "cho(t)-ta" )
        , ( "꼬리", "kko-li" )
        , ( "낚시", "na(k)-shi" )
        , ( "또", "tto" )
        , ( "뛰다", "kwi-ta" )
        , ( "빵", "bbang" )
        , ( "쓰다", "sseu-ta" )
        , ( "맛있다", "ma(t)-i(t)-ta" )
        , ( "찌게", "cchi-gae" )
        , ( "이데일리", "i-tae-il-li" )
        ]


jobQuestions =
    List.map (\( hanguel, english ) -> Question hanguel (JustEnglish english) (Set.fromList [ "Job" ]))
        [ ( "엔지니어", "engineer" )
        , ( "농부", "farmer" )
        , ( "변호사", "lawyer" )
        , ( "시인", "poet" )
        , ( "정원사", "gardener" )
        , ( "가수", "singer" )
        , ( "고등학생", "high school student" )
        , ( "대학생", "university student" )
        , ( "대학원생", "graduate student" )
        , ( "학생", "student" )
        , ( "초듷학생", "primary school student" )
        , ( "중학생", "middle school student" )
        ]


locationQuestions =
    List.map (\( hanguel, english ) -> Question hanguel (JustEnglish english) (Set.fromList [ "Location" ]))
        [ ( "우체국", "post office" )
        , ( "기숙사", "dormitory" )
        , ( "책방", "book store" )
        , ( "위", "above" )
        , ( "아래", "below" )
        , ( "뒤", "behind" )
        , ( "식당", "restaurant" )
        , ( "학교", "school" )
        , ( "대학교", "university" )
        , ( "빌딩", "building" )
        , ( "안", "in" )
        , ( "앞", "front" )
        , ( "어디", "where" )
        , ( "옆", "beside" )
        , ( "학생회관", "student centre" )
        , ( "캠퍼스", "campus" )
        , ( "층", "storey" )
        ]


objectQuestions =
    List.map (\( hanguel, english ) -> Question hanguel (JustEnglish english) (Set.fromList [ "Object" ]))
        [ ( "가방", "bag" )
        , ( "시계", "clock" )
        , ( "의자", "chair" )
        , ( "책상", "desk" )
        , ( "책", "book" )
        , ( "숙제", "homework" )
        , ( "아침", "breakfast/morning" )
        ]


verbQuestions =
    List.map (\( hanguel, english ) -> Question hanguel (JustEnglish english) (Set.fromList [ "Verb" ]))
        [ ( "있어요", "exists/is there" )
        , ( "어때요", "is how" )
        , ( "좋아요", "is good, nice" )
        , ( "나빠요", "is bad" )
        , ( "괜찮아요", "is okay/not bad" )
        , ( "맛있어요", "is delicious" )
        , ( "맛없어요", "tastes bad" )
        , ( "커요", "is big" )
        , ( "많아요", "is much/many" )
        , ( "싸요", "is cheap" )
        , ( "넓어요", "is spacious" )
        , ( "읽어요", "read" )
        , ( "숙제해요", "do homework" )
        , ( "샤워해요", "have a shower" )
        , ( "작아요", "is small" )
        , ( "없어요", "do not have" )
        , ( "봐요", "see" )
        , ( "사랑해요", "love" )
        , ( "써요", "use" )
        , ( "예뻐요", "is pretty" )
        , ( "전화해요", "call/phone" )
        , ( "먹어요", "eat" )
        , ( "마셔요", "drink" )
        , ( "공부해요", "study" )
        , ( "자요", "sleep" )
        , ( "앉아요", "sit down" )
        , ( "비싸요", "is expensive" )
        , ( "아주", "very a..." )
        , ( "참", "very ch..." )
        , ( "그리고", "and also" )
        , ( "그런데", "however" )
        ]


schoolQuestions =
    List.map (\( hanguel, english ) -> Question hanguel (JustEnglish english) (Set.fromList [ "School" ]))
        [ ( "경제학", "economics" )
        , ( "교과서", "textbook" )
        , ( "교실", "classroom" )
        , ( "반", "class" )
        , ( "사전", "dictionary" )
        , ( "수업", "course, class" )
        , ( "여자", "woman" )
        , ( "질문", "question" )
        , ( "우산", "umbrella" )
        , ( "집", "house, home" )
        , ( "친구", "friend" )
        , ( "컴퓨터", "computer" )
        , ( "누구", "who" )
        , ( "인사해유", "greets" )
        , ( "재미있어유", "to be interesting" )
        , ( "많이", "many" )
        , ( "시험", "exam" )
        , ( "남자", "man" )
        , ( "역사", "history" )
        , ( "주스", "juice" )
        , ( "텔레비전", "television" )
        , ( "만나요", "meets" )
        , ( "지내요", "to get along, to be doing" )
        , ( "그래서", "so, therefore" )
        , ( "그럼", "(if so) then" )
        , ( "어떻게", "how" )
        , ( "잘", "well" )
        , ( "재미없어요", "is uninteresting" )
        , ( "열심히", "diligently" )
        , ( "전공해요", "majors in" )
        ]


timeQuestions =
    List.map (\( hanguel, english ) -> Question hanguel (JustEnglish english) (Set.fromList [ "Time" ]))
        [ ( "매일", "every day" )
        , ( "요즘", "these days" )
        , ( "지금", "now" )
        , ( "내일", "tomorrow" )
        , ( "시간", "time" )
        , ( "오늘", "today" )
        ]


greetingQuestions =
    List.map (\( hanguel, english ) -> Question hanguel (JustEnglish english) (Set.fromList [ "Greeting" ]))
        [ ( "어떻게 지내세요?", "How are you doing?" )
        , ( "바빠요", "I am busy" )
        , ( "그저 그래요", "Just so-so" )
        , ( "잘 지내요", "I am doing well" )
        ]


instructionQuestions =
    List.map (\( hanguel, english ) -> Question hanguel (JustEnglish english) (Set.fromList [ "Instruction" ]))
        [ ( "책을 펴세요", "open your book" )
        , ( "책을 덮으세요", "close your book" )
        , ( "칠판을 보세요", "look at the board" )
        , ( "따라하세요", "repeat after me" )
        , ( "잘 들으세요", "listen up" )
        , ( "읽으세요", "read (aloud)" )
        , ( "쓰세요", "write/do" )
        , ( "다시 한 번 말씀해 주세요", "please say that again" )
        , ( "클게 말씀해 주세요", "please speak loudly" )
        , ( "천천히 말씀해 주세요", "please speak slowly" )
        ]


natNumQuestions =
    List.map (\( hanguel, english ) -> Question hanguel (JustEnglish ("native " ++ english)) (Set.fromList [ "Native Number" ]))
        [ ( "하나", "1" )
        , ( "둘", "2" )
        , ( "셋", "3" )
        , ( "넷", "4" )
        , ( "다섯", "5" )
        , ( "여섯", "6" )
        , ( "일곱", "7" )
        , ( "여덟", "8" )
        , ( "아홉", "9" )
        , ( "열", "10" )
        ]


accCounterQuestions =
    List.map (\( hanguel, english ) -> Question hanguel (JustEnglish ("acc " ++ english)) (Set.fromList [ "Counter" ]))
        [ ( "명", "people" )
        , ( "마리", "animals" )
        , ( "개", "items" )
        , ( "권", "volumes (e.g. of books)" )
        , ( "[native] 과", "lessons" )
        , ( "시간", "hours" )
        , ( "달", "months" )
        ]


idCounterQuestions =
    List.map (\( hanguel, english ) -> Question hanguel (JustEnglish ("id " ++ english)) (Set.fromList [ "Counter" ]))
        [ ( "층", "floor, layer" )
        , ( "[sino] 과", "lesson" )
        , ( "원", "won, ie. currency" )
        , ( "학년", "school year" )
        , ( "년", "year" )
        , ( "월", "month" )
        , ( "[sino] 일", "day" )
        ]


familyQuestions =
    List.map (\( hanguel, english ) -> Question hanguel (JustEnglish english) (Set.fromList [ "Family" ]))
        [ ( "남동생", "younger brother" )
        , ( "여동생", "younger sister" )
        , ( "동생", "younger sibling" )
        , ( "오빠", "girl's older brother" )
        , ( "형", "boy's older brother" )
        , ( "누나", "boy's older sister" )
        , ( "언니", "girl's older sister" )
        , ( "부모님", "parents" )
        , ( "아버지", "father" )
        , ( "어머니", "mother" )
        ]


lesson4class2 =
    List.map (\( hanguel, english ) -> Question hanguel (JustEnglish english) (Set.fromList [ "L4C2" ]))
        [ ( "거", "thing" )
        , ( "뉴욕", "New York" )
        , ( "룸메이트", "roommate" )
        , ( "방", "room" )
        , ( "사이", "relationship, between" )
        , ( "생물학", "Biology" )
        , ( "아파트", "apartment" )
        , ( "로스앤젤레스", "Los Angeles" )
        , ( "하와이", "Hawaii" )
        , ( "만", "only" )
        , ( "고마워요", "is thankful" )
        , ( "배워요", "learns" )
        , ( "와요", "comes" )
        , ( "줘요", "gives" )
        , ( "저희", "we, us, our" )
        , ( "이거", "this thing" )
        , ( "제", "my" )
        ]


lesson5class1 =
    List.map (\( hanguel, english ) -> Question hanguel (JustEnglish english) (Set.fromList [ "L5C1" ]))
        [ ( "공원", "park" )
        , ( "랩", "lab" )
        , ( "백화점", "department store" )
        , ( "생일", "birthday" )
        , ( "서점", "bookstore" )
        , ( "선물", "present, gift" )
        , ( "쇼핑", "shopping" )
        , ( "연슴", "practice" )
        , ( "오래간만이에요", "it's been a long time" )
        , ( "운동", "exercise" )
        , ( "일", "work" )
        , ( "점심", "lunch" )
        , ( "커피숍", "coffee shop" )
        , ( "테니스", "tennis" )
        , ( "햄버거", "hamburger" )
        , ( "가르쳐요", "teaches" )
        , ( "사요", "buys" )
        , ( "쳐요", "plays" )
        , ( "안녕히", "in peace" )
        ]


lesson5class2 =
    List.map (\( hanguel, english ) -> Question hanguel (JustEnglish english) (Set.fromList [ "L5C2" ]))
        ([ ( "가게", "store" )
         , ( "오전", "AM" )
         , ( "오후", "PM" )
         , ( "옷", "clothes" )
         , ( "정치학", "political science" )
         , ( "이번", "this time" )
         , ( "학기", "semester, academic term" )
         , ( "걷어요", "walks" )
         , ( "들어요", "listens, takes a course" )
         , ( "좋아해요", "likes" )
         , ( "과몰", "course, subject" )
         , ( "분", "minute" )
         , ( "시", "o'clock" )
         , ( "같이", "together" )
         , ( "언제", "when" )
         , ( "에", "at, in, on (time)" )
         , ( "하고", "with" )
         , ( "~(으)러", "in order to" )
         ]
            ++ List.map (\( h, a ) -> ( h ++ "요일", a ))
                [ ( "월", "Monday" )
                , ( "화", "Tuesday" )
                , ( "수", "Wednesday" )
                , ( "목", "Thursday" )
                , ( "금", "Friday" )
                , ( "토", "Saturday" )
                , ( "일", "Sunday" )
                ]
        )
