module Tests exposing (..)

import Date exposing (Date)
import Expect
import Strftime
import Test exposing (Test, describe, test)
import Test.Runner.Html
import Time


main : Test.Runner.Html.TestProgram
main =
    Test.Runner.Html.run all


july02 : Date
july02 =
    -- > Date.fromTime 1499000000000
    -- <Sun Jul 02 2017 08:53:20 GMT-0400 (EDT)> : Date.Date
    Date.fromTime 1499000000000


jan01 : Date
jan01 =
    -- > Date.fromTime 1483246800000
    -- <Sun Jan 01 2017 00:00:00 GMT-0500 (EST)> : Date.Date
    Date.fromTime 1483246800000


all : Test
all =
    describe "All Tests"
        [ test "Blank format gets blank response" <|
            \() ->
                Expect.equal "" <| Strftime.format "" july02
        , dayTests
        , monthTests
        , yearTests
        , hourTests
        , minuteTests
        , secondTests
        ]


dayTests : Test
dayTests =
    describe "Weekday and Day of Month Tests"
        [ test "Abbreviated weekday" <|
            \() ->
                Expect.equal "Sun" <| Strftime.format "%a" july02
        , test "Full weekday" <|
            \() ->
                Expect.equal "Sunday" <| Strftime.format "%A" july02
        , test "Weekday as number" <|
            \() ->
                Expect.equal "0" <| Strftime.format "%w" july02
        , test "Day of month as number" <|
            \() ->
                Expect.equal "2" <| Strftime.format "%-d" july02
        , test "Day of month as zero-padded number" <|
            \() ->
                Expect.equal "02" <| Strftime.format "%d" july02
        , test "Day of year as number" <|
            \() ->
                Expect.equal "183" <| Strftime.format "%-j" july02
        , test "Day of year as zero-padded number" <|
            \() ->
                Expect.equal "183" <| Strftime.format "%j" july02
        , test "Day of year as zero-padded number" <|
            \() ->
                Expect.equal "001" <| Strftime.format "%j" jan01
        ]


monthTests : Test
monthTests =
    describe "Month Tests"
        [ test "Abbreviated month" <|
            \() ->
                Expect.equal "Jul" <| Strftime.format "%b" july02
        , test "Full month" <|
            \() ->
                Expect.equal "July" <| Strftime.format "%B" july02
        ]


yearTests : Test
yearTests =
    describe "Year Tests"
        [ test "Year without century" <|
            \() ->
                Expect.equal "17" <| Strftime.format "%y" july02
        , test "Year with century" <|
            \() ->
                Expect.equal "2017" <| Strftime.format "%Y" july02
        ]


hourTests : Test
hourTests =
    let
        july02Afternoon =
            july02
                |> Date.toTime
                |> (+) (Time.hour * 12)
                |> Date.fromTime

        july02Midnight =
            july02
                |> Date.toTime
                |> flip (-) (Time.hour * 8)
                |> Date.fromTime
    in
        describe "Hour Tests"
            [ test "12-hour zero-padded hour" <|
                \() ->
                    Expect.equal "08" <| Strftime.format "%I" july02
            , test "12-hour hour" <|
                \() ->
                    Expect.equal "8" <| Strftime.format "%-I" july02
            , test "12-hour afternoon" <|
                \() ->
                    Expect.equal "8" <| Strftime.format "%-I" july02Afternoon
            , test "12-hour midnight" <|
                \() ->
                    Expect.equal "12" <| Strftime.format "%-I" july02Midnight
            , test "24-hour zero-padded hour" <|
                \() ->
                    Expect.equal "08" <| Strftime.format "%H" july02
            , test "24-hour hour" <|
                \() ->
                    Expect.equal "8" <| Strftime.format "%-H" july02
            , test "24-hour afternoon" <|
                \() ->
                    Expect.equal "20" <| Strftime.format "%-H" july02Afternoon
            , test "24-hour midnight" <|
                \() ->
                    Expect.equal "00" <| Strftime.format "%H" july02Midnight
            , test "AM" <|
                \() ->
                    Expect.equal "AM" <| Strftime.format "%p" july02
            , test "PM" <|
                \() ->
                    Expect.equal "PM" <| Strftime.format "%p" july02Afternoon
            ]


minuteTests : Test
minuteTests =
    describe "Minute Tests"
        [ test "Zero-padded minutes" <|
            \() ->
                Expect.equal "53" <| Strftime.format "%M" july02
        , test "Minutes" <|
            \() ->
                Expect.equal "53" <| Strftime.format "%-M" july02
        ]


secondTests : Test
secondTests =
    describe "Second Tests"
        [ test "Zero-padded seconds" <|
            \() ->
                Expect.equal "20" <| Strftime.format "%S" july02
        , test "Seconds" <|
            \() ->
                Expect.equal "20" <| Strftime.format "%-S" july02
        ]
