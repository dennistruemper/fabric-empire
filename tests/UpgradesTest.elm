module UpgradesTest exposing (..)

import Expect exposing (FloatingPointTolerance(..))
import Lib.Upgrades as Upgrades
import Set
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Lib.Upgrades"
        [ describe "allUpgrades"
            [ test "has 20 upgrades" <|
                \_ ->
                    List.length Upgrades.allUpgrades
                        |> Expect.equal 20
            , test "each upgrade has unique id" <|
                \_ ->
                    let
                        ids =
                            List.map .id Upgrades.allUpgrades

                        uniqueCount =
                            Set.fromList ids |> Set.size
                    in
                    uniqueCount
                        |> Expect.equal (List.length ids)
            ]
        , describe "fiberClickMultiplier"
            [ test "is 1.0 with no upgrades" <|
                \_ ->
                    Upgrades.fiberClickMultiplier Set.empty
                        |> Expect.within (Absolute 0.001) 1.0
            , test "is 2.0 with sharp-shears" <|
                \_ ->
                    Upgrades.fiberClickMultiplier (Set.singleton "sharp-shears")
                        |> Expect.within (Absolute 0.001) 2.0
            , test "stacks multiplicatively" <|
                \_ ->
                    Upgrades.fiberClickMultiplier (Set.fromList [ "sharp-shears", "silken-touch" ])
                        |> Expect.within (Absolute 0.001) 10.0
            ]
        , describe "fiberProductionMultiplier"
            [ test "is 1.0 with no upgrades" <|
                \_ ->
                    Upgrades.fiberProductionMultiplier Set.empty
                        |> Expect.within (Absolute 0.001) 1.0
            , test "premium-feed gives 2x" <|
                \_ ->
                    Upgrades.fiberProductionMultiplier (Set.singleton "premium-feed")
                        |> Expect.within (Absolute 0.001) 2.0
            , test "golden-needles also applies to fiber" <|
                \_ ->
                    Upgrades.fiberProductionMultiplier (Set.singleton "golden-needles")
                        |> Expect.within (Absolute 0.001) 1.5
            , test "stacks premium-feed and golden-needles" <|
                \_ ->
                    Upgrades.fiberProductionMultiplier (Set.fromList [ "premium-feed", "golden-needles" ])
                        |> Expect.within (Absolute 0.001) 3.0
            ]
        , describe "stitchProductionMultiplier"
            [ test "is 1.0 with no upgrades" <|
                \_ ->
                    Upgrades.stitchProductionMultiplier Set.empty
                        |> Expect.within (Absolute 0.001) 1.0
            , test "ergonomic-hooks gives 2x" <|
                \_ ->
                    Upgrades.stitchProductionMultiplier (Set.singleton "ergonomic-hooks")
                        |> Expect.within (Absolute 0.001) 2.0
            , test "stacks ergonomic-hooks and pattern-book" <|
                \_ ->
                    Upgrades.stitchProductionMultiplier (Set.fromList [ "ergonomic-hooks", "pattern-book" ])
                        |> Expect.within (Absolute 0.001) 4.0
            , test "golden-needles also applies to stitches" <|
                \_ ->
                    Upgrades.stitchProductionMultiplier (Set.fromList [ "ergonomic-hooks", "golden-needles" ])
                        |> Expect.within (Absolute 0.001) 3.0
            ]
        , describe "findById"
            [ test "finds existing upgrade" <|
                \_ ->
                    Upgrades.findById "sharp-shears"
                        |> Maybe.map .name
                        |> Expect.equal (Just "Sharp Shears")
            , test "returns Nothing for unknown id" <|
                \_ ->
                    Upgrades.findById "laser-eyes"
                        |> Expect.equal Nothing
            ]
        ]
