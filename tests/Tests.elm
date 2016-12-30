module Tests exposing (..)

import Date exposing (Date)
import Expect
import Strftime
import Test exposing (Test, describe, test)
import Test.Runner.Html


main : Test.Runner.Html.TestProgram
main =
    Test.Runner.Html.run all


july13 : Date
july13 =
    -- > Date.fromTime 1499000000000
    -- <Sun Jul 02 2017 08:53:20 GMT-0400 (EDT)> : Date.Date
    Date.fromTime 1499000000000


all : Test
all =
    describe "All Tests"
        [ test "Blank format gets blank response" <|
            \() ->
                Expect.equal "" <| Strftime.format "" july13
        , dayTests
        , monthTests
        , yearTests
        , hourTests
        , minuteTests
        ]


dayTests : Test
dayTests =
    describe "Weekday and Day of Month Tests"
        [ test "Abbreviated weekday" <|
            \() ->
                Expect.equal "Sun" <| Strftime.format "%a" july13
        , test "Full weekday" <|
            \() ->
                Expect.equal "Sunday" <| Strftime.format "%A" july13
        , test "Weekday as number" <|
            \() ->
                Expect.equal "0" <| Strftime.format "%w" july13
        , test "Day of month as number" <|
            \() ->
                Expect.equal "2" <| Strftime.format "%-d" july13
        , test "Day of month as zero-padded number" <|
            \() ->
                Expect.equal "02" <| Strftime.format "%d" july13
        ]


monthTests : Test
monthTests =
    describe "Month Tests"
        [ test "Abbreviated month" <|
            \() ->
                Expect.equal "Jul" <| Strftime.format "%b" july13
        , test "Full month" <|
            \() ->
                Expect.equal "July" <| Strftime.format "%B" july13
        ]


yearTests : Test
yearTests =
    describe "Year Tests"
        [ test "Year without century" <|
            \() ->
                Expect.equal "17" <| Strftime.format "%y" july13
        , test "Year with century" <|
            \() ->
                Expect.equal "2017" <| Strftime.format "%Y" july13
        ]


hourTests : Test
hourTests =
    describe "Hour Tests"
        [ test "12-hour zero-padded hour" <|
            \() ->
                Expect.equal "08" <| Strftime.format "%I" july13
        , test "12-hour hour" <|
            \() ->
                Expect.equal "8" <| Strftime.format "%-I" july13
        ]


minuteTests : Test
minuteTests =
    describe "Minute Tests"
        [ test "Zero-padded minutes" <|
            \() ->
                Expect.equal "53" <| Strftime.format "%M" july13
        , test "Minutes" <|
            \() ->
                Expect.equal "53" <| Strftime.format "%-M" july13
        ]
