module ProducersTest exposing (..)

import Expect exposing (FloatingPointTolerance(..))
import Lib.Producers as Producers
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Lib.Producers"
        [ describe "allProducers"
            [ test "has 6 producers" <|
                \_ ->
                    List.length Producers.allProducers
                        |> Expect.equal 6
            , test "each producer has unique id" <|
                \_ ->
                    let
                        ids =
                            List.map .id Producers.allProducers

                        uniqueIds =
                            ids
                                |> List.sort
                                |> dedup
                    in
                    List.length uniqueIds
                        |> Expect.equal (List.length ids)
            , test "producers are ordered by increasing cost" <|
                \_ ->
                    let
                        costs =
                            List.map .baseCost Producers.allProducers
                    in
                    isSorted costs
                        |> Expect.equal True
            ]
        , describe "currentCost"
            [ test "first purchase costs baseCost" <|
                \_ ->
                    case Producers.findById "sheep" of
                        Just sheep ->
                            Producers.currentCost sheep 0
                                |> Expect.within (Absolute 0.01) 15.0

                        Nothing ->
                            Expect.fail "sheep not found"
            , test "cost increases with each purchase" <|
                \_ ->
                    case Producers.findById "sheep" of
                        Just sheep ->
                            let
                                cost0 =
                                    Producers.currentCost sheep 0

                                cost1 =
                                    Producers.currentCost sheep 1

                                cost5 =
                                    Producers.currentCost sheep 5
                            in
                            Expect.all
                                [ \_ -> Expect.greaterThan cost0 cost1
                                , \_ -> Expect.greaterThan cost1 cost5
                                ]
                                ()

                        Nothing ->
                            Expect.fail "sheep not found"
            , test "cost scales by 1.15x per unit owned" <|
                \_ ->
                    case Producers.findById "sheep" of
                        Just sheep ->
                            let
                                cost0 =
                                    Producers.currentCost sheep 0

                                cost1 =
                                    Producers.currentCost sheep 1
                            in
                            Expect.within (Absolute 0.01) (cost0 * 1.15) cost1

                        Nothing ->
                            Expect.fail "sheep not found"
            ]
        , describe "outputPerSecond"
            [ test "zero units produce zero" <|
                \_ ->
                    case Producers.findById "sheep" of
                        Just sheep ->
                            Producers.outputPerSecond sheep 0
                                |> Expect.within (Absolute 0.001) 0

                        Nothing ->
                            Expect.fail "sheep not found"
            , test "output scales linearly with count" <|
                \_ ->
                    case Producers.findById "sheep" of
                        Just sheep ->
                            Producers.outputPerSecond sheep 10
                                |> Expect.within (Absolute 0.001) (sheep.baseOutput * 10)

                        Nothing ->
                            Expect.fail "sheep not found"
            ]
        , describe "findById"
            [ test "finds existing producer" <|
                \_ ->
                    Producers.findById "alpaca"
                        |> Maybe.map .name
                        |> Expect.equal (Just "Alpaca")
            , test "returns Nothing for unknown id" <|
                \_ ->
                    Producers.findById "dragon"
                        |> Expect.equal Nothing
            ]
        ]


{-| Helper: check if a list of comparable values is sorted ascending.
-}
isSorted : List comparable -> Bool
isSorted list =
    case list of
        [] ->
            True

        [ _ ] ->
            True

        a :: b :: rest ->
            (a <= b) && isSorted (b :: rest)


{-| Helper: remove consecutive duplicates from a sorted list.
-}
dedup : List comparable -> List comparable
dedup list =
    case list of
        [] ->
            []

        [ x ] ->
            [ x ]

        a :: b :: rest ->
            if a == b then
                dedup (b :: rest)

            else
                a :: dedup (b :: rest)
