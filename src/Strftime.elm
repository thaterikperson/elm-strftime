module Strftime exposing (format)

{-| An (incomplete) implementation of the strftime format based on rules
from http://strftime.org.

@docs format

-}

import Date exposing (Date, Month(..), Day(..))
import Regex exposing (..)
import Result
import String
import Time


{-| Format a date into a string of your choice. Follow the rules at
http://strftime.org.

Examples

    format "%d %B %y" (Date.fromTime 1499000000000) == "02 July 17"
-}
format : String -> Date -> String
format fmt date =
    fmt
        |> Regex.replace All (regex "%b") (\_ -> abbreviatedMonth date)
        |> Regex.replace All (regex "%B") (\_ -> fullMonth date)
        |> Regex.replace All (regex "%-m") (\_ -> toString <| numericMonth date)
        |> Regex.replace All (regex "%m") (\_ -> zeroPad <| numericMonth date)
        |> Regex.replace All (regex "%a") (\_ -> abbreviatedWeekday date)
        |> Regex.replace All (regex "%A") (\_ -> fullWeekday date)
        |> Regex.replace All (regex "%w") (\_ -> numberWeekday date)
        |> Regex.replace All (regex "%-d") (\_ -> toString <| Date.day date)
        |> Regex.replace All (regex "%d") (\_ -> zeroPad <| Date.day date)
        |> Regex.replace All (regex "%y") (\_ -> String.right 2 <| toString <| Date.year date)
        |> Regex.replace All (regex "%Y") (\_ -> toString <| Date.year date)
        |> Regex.replace All (regex "%-H") (\_ -> toString <| Date.hour date)
        |> Regex.replace All (regex "%H") (\_ -> zeroPad <| Date.hour date)
        |> Regex.replace All (regex "%-I") (\_ -> toString <| twentyFourHourToTwelveHour <| Date.hour date)
        |> Regex.replace All (regex "%I") (\_ -> zeroPad <| twentyFourHourToTwelveHour <| Date.hour date)
        |> Regex.replace All (regex "%p") (\_ -> amPmString date)
        |> Regex.replace All (regex "%-M") (\_ -> toString <| Date.minute date)
        |> Regex.replace All (regex "%M") (\_ -> zeroPad <| Date.minute date)
        |> Regex.replace All (regex "%-S") (\_ -> toString <| Date.second date)
        |> Regex.replace All (regex "%S") (\_ -> zeroPad <| Date.second date)
        |> Regex.replace All (regex "%j") (\_ -> zeroPadThreeSpaces <| dayOfYear date)
        |> Regex.replace All (regex "%-j") (\_ -> toString <| dayOfYear date)


zeroPad : Int -> String
zeroPad number =
    if number < 10 then
        "0" ++ (toString number)
    else
        toString number


zeroPadThreeSpaces : Int -> String
zeroPadThreeSpaces number =
    let
        padded =
            zeroPad number
    in
        if (String.length padded) == 2 then
            String.cons '0' padded
        else
            padded


dayOfYear : Date -> Int
dayOfYear date =
    let
        midnightDate =
            [ Date.year, (Date.month >> monthOfYear), Date.day ]
                |> List.map (\f -> toString <| f date)
                |> String.join "/"
                |> Date.fromString
                |> Result.withDefault (Date.fromTime 0)
    in
        date
            |> Date.year
            |> toString
            |> flip (++) "/01/01"
            |> Date.fromString
            |> Result.map
                (\jan01 ->
                    let
                        diff =
                            (Date.toTime midnightDate) - (Date.toTime jan01)

                        oneDay =
                            Time.hour * 24
                    in
                        (ceiling (diff / oneDay) + 1)
                )
            |> Result.withDefault 0


numberWeekday : Date -> String
numberWeekday date =
    case Date.dayOfWeek date of
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


fullWeekday : Date -> String
fullWeekday date =
    case Date.dayOfWeek date of
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


abbreviatedWeekday : Date -> String
abbreviatedWeekday date =
    case Date.dayOfWeek date of
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


fullMonth : Date -> String
fullMonth date =
    case Date.month date of
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


abbreviatedMonth : Date -> String
abbreviatedMonth date =
    case Date.month date of
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


numericMonth : Date -> Int
numericMonth date =
    case Date.month date of
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


amPmString : Date -> String
amPmString date =
    if (Date.hour date) > 11 then
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
