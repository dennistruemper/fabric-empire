module ProjectsTest exposing (..)

import Expect exposing (FloatingPointTolerance(..))
import Lib.Projects as Projects
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Lib.Projects"
        [ describe "allProjects"
            [ test "has 6 projects" <|
                \_ ->
                    List.length Projects.allProjects
                        |> Expect.equal 6
            , test "projects are ordered by increasing cost" <|
                \_ ->
                    let
                        costs =
                            List.map .baseCost Projects.allProjects
                    in
                    isSorted costs
                        |> Expect.equal True
            ]
        , describe "currentCost"
            [ test "first purchase costs baseCost" <|
                \_ ->
                    case Projects.findById "granny-square" of
                        Just gs ->
                            Projects.currentCost gs 0
                                |> Expect.within (Absolute 0.01) 50.0

                        Nothing ->
                            Expect.fail "granny-square not found"
            , test "cost increases with each purchase" <|
                \_ ->
                    case Projects.findById "scarf" of
                        Just scarf ->
                            let
                                cost0 =
                                    Projects.currentCost scarf 0

                                cost3 =
                                    Projects.currentCost scarf 3
                            in
                            Expect.greaterThan cost0 cost3

                        Nothing ->
                            Expect.fail "scarf not found"
            ]
        , describe "outputPerSecond"
            [ test "zero projects produce zero stitches" <|
                \_ ->
                    case Projects.findById "scarf" of
                        Just scarf ->
                            Projects.outputPerSecond scarf 0
                                |> Expect.within (Absolute 0.001) 0

                        Nothing ->
                            Expect.fail "scarf not found"
            , test "output scales linearly" <|
                \_ ->
                    case Projects.findById "scarf" of
                        Just scarf ->
                            Projects.outputPerSecond scarf 5
                                |> Expect.within (Absolute 0.001) (scarf.baseOutput * 5)

                        Nothing ->
                            Expect.fail "scarf not found"
            ]
        , describe "findById"
            [ test "finds existing project" <|
                \_ ->
                    Projects.findById "beanie"
                        |> Maybe.map .name
                        |> Expect.equal (Just "Beanie")
            , test "returns Nothing for unknown id" <|
                \_ ->
                    Projects.findById "hat"
                        |> Expect.equal Nothing
            ]
        ]


isSorted : List comparable -> Bool
isSorted list =
    case list of
        [] ->
            True

        [ _ ] ->
            True

        a :: b :: rest ->
            (a <= b) && isSorted (b :: rest)
