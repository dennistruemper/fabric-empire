module GameStateTest exposing (..)

import Dict
import Expect exposing (FloatingPointTolerance(..))
import Json.Decode as Decode
import Lib.GameState as GameState
import Set
import Test exposing (Test, describe, test)
import Time


suite : Test
suite =
    describe "Lib.GameState"
        [ describe "init"
            [ test "starts with zero fiber" <|
                \_ ->
                    GameState.init.fiber
                        |> Expect.within (Absolute 0.001) 0
            , test "starts with zero stitches" <|
                \_ ->
                    GameState.init.stitches
                        |> Expect.within (Absolute 0.001) 0
            , test "starts with no producers" <|
                \_ ->
                    Dict.isEmpty GameState.init.producers
                        |> Expect.equal True
            , test "starts with no upgrades" <|
                \_ ->
                    Set.isEmpty GameState.init.upgrades
                        |> Expect.equal True
            ]
        , describe "click"
            [ test "adds fiber on click" <|
                \_ ->
                    let
                        state =
                            GameState.click GameState.init
                    in
                    Expect.greaterThan 0 state.fiber
            , test "increments click count" <|
                \_ ->
                    let
                        state =
                            GameState.init
                                |> GameState.click
                                |> GameState.click
                                |> GameState.click
                    in
                    state.totalClicks
                        |> Expect.equal 3
            , test "tracks total fiber earned" <|
                \_ ->
                    let
                        state =
                            GameState.click GameState.init
                    in
                    Expect.within (Absolute 0.001) state.fiber state.totalFiberEarned
            ]
        , describe "fiberPerClick"
            [ test "base fiber per click is 1" <|
                \_ ->
                    GameState.fiberPerClick GameState.init
                        |> Expect.within (Absolute 0.001) 1.0
            , test "upgrades multiply click output" <|
                \_ ->
                    let
                        state =
                            { gameWithFiber | upgrades = Set.singleton "sharp-shears" }
                    in
                    GameState.fiberPerClick state
                        |> Expect.within (Absolute 0.001) 2.0
            ]
        , describe "tick"
            [ test "first tick just sets lastTick (no production)" <|
                \_ ->
                    let
                        now =
                            Time.millisToPosix 1000

                        state =
                            GameState.tick now GameState.init
                    in
                    Expect.all
                        [ \s -> Expect.within (Absolute 0.001) 0 s.fiber
                        , \s -> Expect.equal (Time.posixToMillis now) (Time.posixToMillis s.lastTick)
                        ]
                        state
            , test "subsequent tick adds production" <|
                \_ ->
                    let
                        t0 =
                            Time.millisToPosix 1000

                        t1 =
                            Time.millisToPosix 2000

                        stateWithSheep =
                            { gameWithFiber
                                | producers = Dict.singleton "sheep" 10
                                , lastTick = t0
                            }

                        afterTick =
                            GameState.tick t1 stateWithSheep
                    in
                    -- 10 sheep * 0.1/s * 1s = 1.0 fiber
                    Expect.greaterThan stateWithSheep.fiber afterTick.fiber
            , test "tick produces correct fiber amount" <|
                \_ ->
                    let
                        t0 =
                            Time.millisToPosix 0

                        t1 =
                            Time.millisToPosix 1000

                        state =
                            { gameWithFiber
                                | producers = Dict.singleton "sheep" 10
                                , lastTick = t0
                                , fiber = 100
                            }

                        -- Need lastTick != 0 for production
                        stateReady =
                            { state | lastTick = Time.millisToPosix 5000 }

                        afterTick =
                            GameState.tick (Time.millisToPosix 6000) stateReady
                    in
                    -- 10 sheep * 0.1/s * 1s = 1.0 fiber gained
                    Expect.within (Absolute 0.01) 101.0 afterTick.fiber
            ]
        , describe "buyProducer"
            [ test "deducts fiber and adds producer" <|
                \_ ->
                    let
                        state =
                            GameState.buyProducer "sheep" gameWithFiber
                    in
                    Expect.all
                        [ \s -> Expect.lessThan gameWithFiber.fiber s.fiber
                        , \s ->
                            Dict.get "sheep" s.producers
                                |> Maybe.withDefault 0
                                |> Expect.equal 1
                        ]
                        state
            , test "cannot buy if not enough fiber" <|
                \_ ->
                    let
                        state =
                            GameState.buyProducer "sheep" GameState.init
                    in
                    Dict.get "sheep" state.producers
                        |> Maybe.withDefault 0
                        |> Expect.equal 0
            , test "buying increases subsequent cost" <|
                \_ ->
                    let
                        state =
                            GameState.buyProducer "sheep" gameWithFiber

                        costBefore =
                            GameState.producerCost "sheep" gameWithFiber

                        costAfter =
                            GameState.producerCost "sheep" state
                    in
                    Expect.greaterThan costBefore costAfter
            ]
        , describe "startProject"
            [ test "deducts fiber and adds project" <|
                \_ ->
                    let
                        state =
                            GameState.startProject "granny-square" gameWithFiber
                    in
                    Expect.all
                        [ \s -> Expect.lessThan gameWithFiber.fiber s.fiber
                        , \s ->
                            Dict.get "granny-square" s.projects
                                |> Maybe.withDefault 0
                                |> Expect.equal 1
                        ]
                        state
            , test "cannot start if not enough fiber" <|
                \_ ->
                    let
                        state =
                            GameState.startProject "granny-square" GameState.init
                    in
                    Dict.get "granny-square" state.projects
                        |> Maybe.withDefault 0
                        |> Expect.equal 0
            ]
        , describe "buyUpgrade"
            [ test "deducts stitches and adds upgrade" <|
                \_ ->
                    let
                        state =
                            GameState.buyUpgrade "sharp-shears" gameWithStitches
                    in
                    Expect.all
                        [ \s -> Expect.lessThan gameWithStitches.stitches s.stitches
                        , \s -> Set.member "sharp-shears" s.upgrades |> Expect.equal True
                        ]
                        state
            , test "cannot buy same upgrade twice" <|
                \_ ->
                    let
                        state1 =
                            GameState.buyUpgrade "sharp-shears" gameWithStitches

                        stitchesAfterFirst =
                            state1.stitches

                        state2 =
                            GameState.buyUpgrade "sharp-shears" state1
                    in
                    -- Stitches should not change on second attempt
                    Expect.within (Absolute 0.001) stitchesAfterFirst state2.stitches
            , test "cannot buy if not enough stitches" <|
                \_ ->
                    let
                        state =
                            GameState.buyUpgrade "sharp-shears" GameState.init
                    in
                    Set.member "sharp-shears" state.upgrades
                        |> Expect.equal False
            ]
        , describe "fiberPerSecond"
            [ test "is 0 with no producers" <|
                \_ ->
                    GameState.fiberPerSecond GameState.init
                        |> Expect.within (Absolute 0.001) 0
            , test "calculates from producers" <|
                \_ ->
                    let
                        state =
                            { gameWithFiber | producers = Dict.singleton "sheep" 5 }
                    in
                    -- 5 sheep * 0.1/s = 0.5/s (with achievement bonus ~1.0)
                    GameState.fiberPerSecond state
                        |> Expect.within (Absolute 0.01) 0.5
            ]
        , describe "stitchesPerSecond"
            [ test "is 0 with no projects" <|
                \_ ->
                    GameState.stitchesPerSecond GameState.init
                        |> Expect.within (Absolute 0.001) 0
            , test "calculates from projects" <|
                \_ ->
                    let
                        state =
                            { gameWithFiber | projects = Dict.singleton "scarf" 3 }
                    in
                    -- 3 scarves * 1/s = 3/s
                    GameState.stitchesPerSecond state
                        |> Expect.within (Absolute 0.01) 3.0
            ]
        , describe "checkAchievements"
            [ test "unlocks first-fiber after gathering" <|
                \_ ->
                    let
                        state =
                            { gameWithFiber | totalFiberEarned = 10 }
                                |> GameState.checkAchievements
                    in
                    Set.member "first-fiber" state.achievements
                        |> Expect.equal True
            , test "unlocks first-flock after buying sheep" <|
                \_ ->
                    let
                        state =
                            { gameWithFiber | producers = Dict.singleton "sheep" 1 }
                                |> GameState.checkAchievements
                    in
                    Set.member "first-flock" state.achievements
                        |> Expect.equal True
            , test "does not unlock achievement before threshold" <|
                \_ ->
                    let
                        state =
                            GameState.checkAchievements GameState.init
                    in
                    Set.isEmpty state.achievements
                        |> Expect.equal True
            ]
        , describe "JSON round-trip"
            [ test "encode then decode returns equivalent state" <|
                \_ ->
                    let
                        state =
                            { gameWithFiber
                                | producers = Dict.fromList [ ( "sheep", 5 ), ( "alpaca", 2 ) ]
                                , projects = Dict.singleton "scarf" 3
                                , upgrades = Set.fromList [ "sharp-shears", "premium-feed" ]
                                , achievements = Set.fromList [ "first-fiber", "first-flock" ]
                                , totalClicks = 42
                                , stitches = 500
                                , totalStitchesEarned = 500
                                , lastTick = Time.millisToPosix 123456
                            }

                        result =
                            GameState.encode state
                                |> Decode.decodeValue GameState.decoder
                    in
                    case result of
                        Ok decoded ->
                            Expect.all
                                [ \s -> Expect.within (Absolute 0.001) state.fiber s.fiber
                                , \s -> Expect.within (Absolute 0.001) state.stitches s.stitches
                                , \s -> Expect.equal state.producers s.producers
                                , \s -> Expect.equal state.projects s.projects
                                , \s -> Expect.equal state.upgrades s.upgrades
                                , \s -> Expect.equal state.achievements s.achievements
                                , \s -> Expect.equal state.totalClicks s.totalClicks
                                , \s -> Expect.equal (Time.posixToMillis state.lastTick) (Time.posixToMillis s.lastTick)
                                ]
                                decoded

                        Err err ->
                            Expect.fail ("Decode failed: " ++ Decode.errorToString err)
            , test "decode empty init state" <|
                \_ ->
                    let
                        result =
                            GameState.encode GameState.init
                                |> Decode.decodeValue GameState.decoder
                    in
                    case result of
                        Ok decoded ->
                            Expect.within (Absolute 0.001) 0 decoded.fiber

                        Err err ->
                            Expect.fail ("Decode failed: " ++ Decode.errorToString err)
            ]
        , describe "canAfford checks"
            [ test "canAffordProducer with enough fiber" <|
                \_ ->
                    GameState.canAffordProducer "sheep" gameWithFiber
                        |> Expect.equal True
            , test "canAffordProducer with no fiber" <|
                \_ ->
                    GameState.canAffordProducer "sheep" GameState.init
                        |> Expect.equal False
            , test "canAffordUpgrade with enough stitches" <|
                \_ ->
                    GameState.canAffordUpgrade "sharp-shears" gameWithStitches
                        |> Expect.equal True
            , test "canAffordUpgrade with no stitches" <|
                \_ ->
                    GameState.canAffordUpgrade "sharp-shears" GameState.init
                        |> Expect.equal False
            ]
        ]



-- TEST HELPERS


{-| A game state with plenty of fiber for purchasing.
-}
gameWithFiber : GameState.GameState
gameWithFiber =
    let
        base =
            GameState.init
    in
    { base | fiber = 1000000, totalFiberEarned = 1000000 }


{-| A game state with plenty of stitches for buying upgrades.
-}
gameWithStitches : GameState.GameState
gameWithStitches =
    let
        base =
            GameState.init
    in
    { base | stitches = 1000000, totalStitchesEarned = 1000000 }
