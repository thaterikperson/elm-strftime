module Strftime exposing (format)

{-| An (incomplete) implementation of the strftime format based on rules
from http://strftime.org.

@docs format

-}

import Date exposing (Date, Month(..), Day(..))
import Regex exposing (..)
import String


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
        |> Regex.replace All (regex "%a") (\_ -> abbreviatedWeekday date)
        |> Regex.replace All (regex "%A") (\_ -> fullWeekday date)
        |> Regex.replace All (regex "%w") (\_ -> numberWeekday date)
        |> Regex.replace All (regex "%-d") (\_ -> toString <| Date.day date)
        |> Regex.replace All (regex "%d") (\_ -> zeroPad <| Date.day date)
        |> Regex.replace All (regex "%y") (\_ -> String.right 2 <| toString <| Date.year date)
        |> Regex.replace All (regex "%Y") (\_ -> toString <| Date.year date)
        |> Regex.replace All (regex "%-I") (\_ -> toString <| Date.hour date)
        |> Regex.replace All (regex "%I") (\_ -> zeroPad <| Date.hour date)
        |> Regex.replace All (regex "%-M") (\_ -> toString <| Date.minute date)
        |> Regex.replace All (regex "%M") (\_ -> zeroPad <| Date.minute date)


zeroPad : Int -> String
zeroPad number =
    if number < 10 then
        "0" ++ (toString number)
    else
        toString number


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
