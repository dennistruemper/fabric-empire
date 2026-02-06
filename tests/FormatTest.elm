module FormatTest exposing (..)

import Expect
import Lib.Format exposing (formatNumber, formatRate)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Lib.Format"
        [ describe "formatNumber"
            [ test "formats zero" <|
                \_ ->
                    formatNumber 0
                        |> Expect.equal "0"
            , test "formats small integer" <|
                \_ ->
                    formatNumber 5
                        |> Expect.equal "5"
            , test "formats small decimal" <|
                \_ ->
                    formatNumber 1.5
                        |> Expect.equal "1.5"
            , test "formats two-digit number" <|
                \_ ->
                    formatNumber 42
                        |> Expect.equal "42"
            , test "formats three-digit number" <|
                \_ ->
                    formatNumber 123
                        |> Expect.equal "123"
            , test "formats thousands with K suffix" <|
                \_ ->
                    formatNumber 1234
                        |> Expect.equal "1.23K"
            , test "formats ten thousands with K suffix" <|
                \_ ->
                    formatNumber 12345
                        |> Expect.equal "12.3K"
            , test "formats hundred thousands with K suffix" <|
                \_ ->
                    formatNumber 123456
                        |> Expect.equal "123K"
            , test "formats millions with M suffix" <|
                \_ ->
                    formatNumber 1234567
                        |> Expect.equal "1.23M"
            , test "formats billions with B suffix" <|
                \_ ->
                    formatNumber 1.5e9
                        |> Expect.equal "1.5B"
            , test "formats trillions with T suffix" <|
                \_ ->
                    formatNumber 2.0e12
                        |> Expect.equal "2T"
            , test "formats negative numbers" <|
                \_ ->
                    formatNumber -500
                        |> Expect.equal "-500"
            ]
        , describe "formatRate"
            [ test "appends /s suffix" <|
                \_ ->
                    formatRate 42
                        |> Expect.equal "42/s"
            , test "formats large rate" <|
                \_ ->
                    formatRate 1500
                        |> Expect.equal "1.5K/s"
            ]
        ]
