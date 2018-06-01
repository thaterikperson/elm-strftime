module Strftime exposing (format)

{-| An (incomplete) implementation of the strftime format based on rules
from <http://strftime.org>.

@docs format

-}

import Regex
import Result
import String
import Time exposing (Month(..), Posix, Weekday(..), Zone)


{-| Format a time into a string of your choice. Follow the rules at
<http://strftime.org>.

Examples

    Strftime.format "%d %B %y" Time.utc (Time.millisToPosix 1499000000000) == "02 July 17"

-}
format : String -> Zone -> Posix -> String
format fmt zone time =
    fmt
        |> Regex.replace (regex "%b") (\_ -> abbreviatedMonth zone time)
        |> Regex.replace (regex "%B") (\_ -> fullMonth zone time)
        |> Regex.replace (regex "%-m") (\_ -> String.fromInt <| numericMonth zone time)
        |> Regex.replace (regex "%m") (\_ -> zeroPad <| numericMonth zone time)
        |> Regex.replace (regex "%a") (\_ -> abbreviatedWeekday zone time)
        |> Regex.replace (regex "%A") (\_ -> fullWeekday zone time)
        |> Regex.replace (regex "%w") (\_ -> numberWeekday zone time)
        |> Regex.replace (regex "%-d") (\_ -> String.fromInt <| Time.toDay zone time)
        |> Regex.replace (regex "%d") (\_ -> zeroPad <| Time.toDay zone time)
        |> Regex.replace (regex "%y") (\_ -> String.right 2 <| String.fromInt <| Time.toYear zone time)
        |> Regex.replace (regex "%Y") (\_ -> String.fromInt <| Time.toYear zone time)
        |> Regex.replace (regex "%-H") (\_ -> String.fromInt <| Time.toHour zone time)
        |> Regex.replace (regex "%H") (\_ -> zeroPad <| Time.toHour zone time)
        |> Regex.replace (regex "%-I") (\_ -> String.fromInt <| twentyFourHourToTwelveHour <| Time.toHour zone time)
        |> Regex.replace (regex "%I") (\_ -> zeroPad <| twentyFourHourToTwelveHour <| Time.toHour zone time)
        |> Regex.replace (regex "%p") (\_ -> amPmString zone time)
        |> Regex.replace (regex "%-M") (\_ -> String.fromInt <| Time.toMinute zone time)
        |> Regex.replace (regex "%M") (\_ -> zeroPad <| Time.toMinute zone time)
        |> Regex.replace (regex "%-S") (\_ -> String.fromInt <| Time.toSecond zone time)
        |> Regex.replace (regex "%S") (\_ -> zeroPad <| Time.toSecond zone time)


regex : String -> Regex.Regex
regex =
    Regex.fromString >> Maybe.withDefault Regex.never


zeroPad : Int -> String
zeroPad number =
    if number < 10 then
        "0" ++ String.fromInt number

    else
        String.fromInt number


zeroPadThreeSpaces : Int -> String
zeroPadThreeSpaces number =
    let
        padded =
            zeroPad number
    in
    if String.length padded == 2 then
        String.cons '0' padded

    else
        padded


numberWeekday : Zone -> Posix -> String
numberWeekday zone time =
    case Time.toWeekday zone time of
        Sun ->
            "0"

        Mon ->
            "1"

        Tue ->
            "2"

        Wed ->
            "3"

        Thu ->
            "4"

        Fri ->
            "5"

        Sat ->
            "6"


fullWeekday : Zone -> Posix -> String
fullWeekday zone time =
    case Time.toWeekday zone time of
        Mon ->
            "Monday"

        Tue ->
            "Tuesday"

        Wed ->
            "Wednesday"

        Thu ->
            "Thursday"

        Fri ->
            "Friday"

        Sat ->
            "Saturday"

        Sun ->
            "Sunday"


abbreviatedWeekday : Zone -> Posix -> String
abbreviatedWeekday zone time =
    case Time.toWeekday zone time of
        Mon ->
            "Mon"

        Tue ->
            "Tue"

        Wed ->
            "Wed"

        Thu ->
            "Thu"

        Fri ->
            "Fri"

        Sat ->
            "Sat"

        Sun ->
            "Sun"


fullMonth : Zone -> Posix -> String
fullMonth zone time =
    case Time.toMonth zone time of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


abbreviatedMonth : Zone -> Posix -> String
abbreviatedMonth zone time =
    case Time.toMonth zone time of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"


numericMonth : Zone -> Posix -> Int
numericMonth zone time =
    case Time.toMonth zone time of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


twentyFourHourToTwelveHour : Int -> Int
twentyFourHourToTwelveHour hour =
    if hour == 0 then
        12

    else if hour > 12 then
        hour - 12

    else
        hour


amPmString : Zone -> Posix -> String
amPmString zone time =
    if Time.toHour zone time > 11 then
        "PM"

    else
        "AM"


monthOfYear : Month -> Int
monthOfYear month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12
