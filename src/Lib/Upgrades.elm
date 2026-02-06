module Lib.Upgrades exposing
    ( Upgrade
    , UpgradeEffect(..)
    , allUpgrades
    , fiberClickMultiplier
    , fiberProductionMultiplier
    , findById
    , stitchProductionMultiplier
    )

{-| Upgrade definitions for the Fiber Empire game.
    Upgrades are one-time purchases (bought with Stitches) that boost production.
-}

import Set exposing (Set)


type alias Upgrade =
    { id : String
    , name : String
    , description : String
    , cost : Float
    , effect : UpgradeEffect
    }


type UpgradeEffect
    = MultiplyFiberPerClick Float
    | MultiplyFiberProduction Float
    | MultiplyStitchProduction Float
    | MultiplyAllProduction Float


allUpgrades : List Upgrade
allUpgrades =
    [ -- ── Tier 1: Early game (50–50K stitches) ──
      { id = "sharp-shears"
      , name = "Sharp Shears"
      , description = "Better tools for gathering. 2x fiber per click."
      , cost = 50
      , effect = MultiplyFiberPerClick 2
      }
    , { id = "premium-feed"
      , name = "Premium Feed"
      , description = "Happy animals, better fiber. 2x fiber production."
      , cost = 200
      , effect = MultiplyFiberProduction 2
      }
    , { id = "ergonomic-hooks"
      , name = "Ergonomic Hooks"
      , description = "Comfortable crafting. 2x stitch production."
      , cost = 500
      , effect = MultiplyStitchProduction 2
      }
    , { id = "pattern-book"
      , name = "Pattern Book"
      , description = "A library of patterns. 2x stitch production."
      , cost = 1500
      , effect = MultiplyStitchProduction 2
      }
    , { id = "golden-needles"
      , name = "Golden Needles"
      , description = "Legendary tools. 1.5x all production."
      , cost = 5000
      , effect = MultiplyAllProduction 1.5
      }
    , { id = "silken-touch"
      , name = "Silken Touch"
      , description = "Masterful hands. 5x fiber per click."
      , cost = 15000
      , effect = MultiplyFiberPerClick 5
      }
    , { id = "master-spinner"
      , name = "Master Spinner"
      , description = "Ancient spinning technique. 3x fiber production."
      , cost = 50000
      , effect = MultiplyFiberProduction 3
      }

    -- ── Tier 2: Mid game (100K–500K stitches) ──
    , { id = "merino-blend"
      , name = "Merino Blend"
      , description = "Premium wool blend. 3x stitch production."
      , cost = 100000
      , effect = MultiplyStitchProduction 3
      }
    , { id = "turbo-carder"
      , name = "Turbo Carder"
      , description = "High-speed fiber carding machine. 5x fiber production."
      , cost = 250000
      , effect = MultiplyFiberProduction 5
      }
    , { id = "diamond-hook"
      , name = "Diamond Hook"
      , description = "A crochet hook that never dulls. 10x fiber per click."
      , cost = 500000
      , effect = MultiplyFiberPerClick 10
      }

    -- ── Tier 3: Late game (1M–10M stitches) ──
    , { id = "artisan-dyes"
      , name = "Artisan Dyes"
      , description = "Hand-mixed dyes that inspire crafters. 2x all production."
      , cost = 1000000
      , effect = MultiplyAllProduction 2
      }
    , { id = "enchanted-loom"
      , name = "Enchanted Loom"
      , description = "A loom woven from moonlight. 5x stitch production."
      , cost = 2500000
      , effect = MultiplyStitchProduction 5
      }
    , { id = "industrial-shears"
      , name = "Industrial Shears"
      , description = "Shears forged from stardust. 25x fiber per click."
      , cost = 5000000
      , effect = MultiplyFiberPerClick 25
      }
    , { id = "angora-revolution"
      , name = "Angora Revolution"
      , description = "The softest fiber known. 5x fiber production."
      , cost = 10000000
      , effect = MultiplyFiberProduction 5
      }

    -- ── Tier 4: End game (25M–500M stitches) ──
    , { id = "celestial-thread"
      , name = "Celestial Thread"
      , description = "Thread spun from starlight. 3x all production."
      , cost = 25000000
      , effect = MultiplyAllProduction 3
      }
    , { id = "cosmic-yarn"
      , name = "Cosmic Yarn"
      , description = "Yarn that transcends dimensions. 10x fiber production."
      , cost = 75000000
      , effect = MultiplyFiberProduction 10
      }
    , { id = "stitch-singularity"
      , name = "Stitch Singularity"
      , description = "Every stitch creates ten more. 10x stitch production."
      , cost = 150000000
      , effect = MultiplyStitchProduction 10
      }
    , { id = "infinite-bobbin"
      , name = "Infinite Bobbin"
      , description = "A bobbin that never runs out. 5x all production."
      , cost = 400000000
      , effect = MultiplyAllProduction 5
      }
    , { id = "fiber-ascension"
      , name = "Fiber Ascension"
      , description = "Your touch turns air into fiber. 50x fiber per click."
      , cost = 1000000000
      , effect = MultiplyFiberPerClick 50
      }
    , { id = "the-grand-pattern"
      , name = "The Grand Pattern"
      , description = "The pattern that weaves the universe. 10x all production."
      , cost = 2500000000
      , effect = MultiplyAllProduction 10
      }
    ]


{-| Calculate the combined fiber-per-click multiplier from all owned upgrades.
-}
fiberClickMultiplier : Set String -> Float
fiberClickMultiplier owned =
    allUpgrades
        |> List.filter (\u -> Set.member u.id owned)
        |> List.foldl
            (\u acc ->
                case u.effect of
                    MultiplyFiberPerClick mult ->
                        acc * mult

                    _ ->
                        acc
            )
            1.0


{-| Calculate the combined fiber production multiplier from all owned upgrades.
-}
fiberProductionMultiplier : Set String -> Float
fiberProductionMultiplier owned =
    allUpgrades
        |> List.filter (\u -> Set.member u.id owned)
        |> List.foldl
            (\u acc ->
                case u.effect of
                    MultiplyFiberProduction mult ->
                        acc * mult

                    MultiplyAllProduction mult ->
                        acc * mult

                    _ ->
                        acc
            )
            1.0


{-| Calculate the combined stitch production multiplier from all owned upgrades.
-}
stitchProductionMultiplier : Set String -> Float
stitchProductionMultiplier owned =
    allUpgrades
        |> List.filter (\u -> Set.member u.id owned)
        |> List.foldl
            (\u acc ->
                case u.effect of
                    MultiplyStitchProduction mult ->
                        acc * mult

                    MultiplyAllProduction mult ->
                        acc * mult

                    _ ->
                        acc
            )
            1.0


{-| Find an upgrade by its string ID.
-}
findById : String -> Maybe Upgrade
findById id =
    allUpgrades
        |> List.filter (\u -> u.id == id)
        |> List.head
