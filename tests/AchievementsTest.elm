module AchievementsTest exposing (..)

import Dict
import Expect
import Lib.Achievements as Achievements exposing (CheckType(..), GameStats)
import Set
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Lib.Achievements"
        [ describe "isUnlocked"
            [ test "TotalFiber unlocks at threshold" <|
                \_ ->
                    Achievements.isUnlocked (TotalFiber 100) { emptyStats | totalFiberEarned = 100 }
                        |> Expect.equal True
            , test "TotalFiber does not unlock below threshold" <|
                \_ ->
                    Achievements.isUnlocked (TotalFiber 100) { emptyStats | totalFiberEarned = 99 }
                        |> Expect.equal False
            , test "TotalStitches unlocks at threshold" <|
                \_ ->
                    Achievements.isUnlocked (TotalStitches 50) { emptyStats | totalStitchesEarned = 50 }
                        |> Expect.equal True
            , test "TotalClicks unlocks at threshold" <|
                \_ ->
                    Achievements.isUnlocked (TotalClicks 10) { emptyStats | totalClicks = 10 }
                        |> Expect.equal True
            , test "ProducerCount checks specific producer" <|
                \_ ->
                    Achievements.isUnlocked (ProducerCount "sheep" 5)
                        { emptyStats | producers = Dict.singleton "sheep" 5 }
                        |> Expect.equal True
            , test "ProducerCount fails if not enough" <|
                \_ ->
                    Achievements.isUnlocked (ProducerCount "sheep" 5)
                        { emptyStats | producers = Dict.singleton "sheep" 3 }
                        |> Expect.equal False
            , test "TotalProducerTypes counts distinct owned types" <|
                \_ ->
                    Achievements.isUnlocked (TotalProducerTypes 2)
                        { emptyStats
                            | producers =
                                Dict.fromList [ ( "sheep", 3 ), ( "alpaca", 1 ) ]
                        }
                        |> Expect.equal True
            , test "TotalProducerTypes ignores zero-count entries" <|
                \_ ->
                    Achievements.isUnlocked (TotalProducerTypes 2)
                        { emptyStats
                            | producers =
                                Dict.fromList [ ( "sheep", 3 ), ( "alpaca", 0 ) ]
                        }
                        |> Expect.equal False
            , test "TotalProjectTypes counts distinct owned types" <|
                \_ ->
                    Achievements.isUnlocked (TotalProjectTypes 1)
                        { emptyStats
                            | projects =
                                Dict.singleton "scarf" 2
                        }
                        |> Expect.equal True
            ]
        , describe "productionBonus"
            [ test "is 1.0 with no achievements" <|
                \_ ->
                    Achievements.productionBonus Set.empty
                        |> Expect.within (Expect.Absolute 0.001) 1.0
            , test "is 1.01 with one achievement (1% per achievement)" <|
                \_ ->
                    Achievements.productionBonus (Set.singleton "first-fiber")
                        |> Expect.within (Expect.Absolute 0.001) 1.01
            , test "is 1.05 with five achievements" <|
                \_ ->
                    Achievements.productionBonus
                        (Set.fromList [ "a", "b", "c", "d", "e" ])
                        |> Expect.within (Expect.Absolute 0.001) 1.05
            ]
        , describe "allAchievements"
            [ test "has achievements defined" <|
                \_ ->
                    List.length Achievements.allAchievements
                        |> Expect.greaterThan 0
            , test "each achievement has unique id" <|
                \_ ->
                    let
                        ids =
                            List.map .id Achievements.allAchievements

                        uniqueCount =
                            Set.fromList ids |> Set.size
                    in
                    uniqueCount
                        |> Expect.equal (List.length ids)
            ]
        ]



-- TEST HELPERS


emptyStats : GameStats
emptyStats =
    { totalFiberEarned = 0
    , totalStitchesEarned = 0
    , totalClicks = 0
    , producers = Dict.empty
    , projects = Dict.empty
    }
