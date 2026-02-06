module Lib.Format exposing (formatNumber, formatRate)

{-| Number formatting utilities for the game.
    Formats large numbers with K, M, B, T, Qa suffixes.
-}


formatNumber : Float -> String
formatNumber n =
    if n < 0 then
        "-" ++ formatNumber (negate n)

    else if n >= 1.0e15 then
        formatSuffix n 1.0e15 "Qa"

    else if n >= 1.0e12 then
        formatSuffix n 1.0e12 "T"

    else if n >= 1.0e9 then
        formatSuffix n 1.0e9 "B"

    else if n >= 1.0e6 then
        formatSuffix n 1.0e6 "M"

    else if n >= 1000 then
        formatSuffix n 1000 "K"

    else if n >= 100 then
        String.fromInt (round n)

    else if n == 0 then
        "0"

    else
        let
            rounded =
                toFloat (round (n * 10)) / 10
        in
        if rounded == toFloat (round rounded) then
            String.fromInt (round rounded)

        else
            String.fromFloat rounded


formatRate : Float -> String
formatRate n =
    formatNumber n ++ "/s"



-- INTERNAL


formatSuffix : Float -> Float -> String -> String
formatSuffix n divisor suffix =
    let
        value =
            n / divisor
    in
    if value >= 100 then
        String.fromInt (round value) ++ suffix

    else if value >= 10 then
        let
            r =
                toFloat (round (value * 10)) / 10
        in
        String.fromFloat r ++ suffix

    else
        let
            r =
                toFloat (round (value * 100)) / 100
        in
        String.fromFloat r ++ suffix
