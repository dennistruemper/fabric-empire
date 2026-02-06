module Lib.Achievements exposing
    ( Achievement
    , CheckType(..)
    , GameStats
    , Progress
    , allAchievements
    , hintFor
    , isUnlocked
    , productionBonus
    , progressFor
    )

{-| Achievement / milestone system for the Fiber Empire game.
    Achievements unlock at thresholds and provide a small production bonus.
-}

import Dict exposing (Dict)
import Set exposing (Set)


{-| Subset of game state needed for achievement checking.
    Avoids circular dependency with GameState module.
-}
type alias GameStats =
    { totalFiberEarned : Float
    , totalStitchesEarned : Float
    , totalClicks : Int
    , producers : Dict String Int
    , projects : Dict String Int
    }


type alias Achievement =
    { id : String
    , name : String
    , description : String
    , check : CheckType
    }


type CheckType
    = TotalFiber Float
    | TotalStitches Float
    | TotalClicks Int
    | ProducerCount String Int
    | ProjectCount String Int
    | TotalProducerTypes Int
    | TotalProjectTypes Int


type alias Progress =
    { current : Float
    , target : Float
    }


allAchievements : List Achievement
allAchievements =
    [ -- ── Fiber milestones ──
      { id = "first-fiber"
      , name = "First Fiber"
      , description = "Gather your very first fiber."
      , check = TotalFiber 1
      }
    , { id = "getting-started"
      , name = "Getting Started"
      , description = "100 fiber gathered!"
      , check = TotalFiber 100
      }
    , { id = "fiber-enthusiast"
      , name = "Fiber Enthusiast"
      , description = "10,000 fiber and counting."
      , check = TotalFiber 10000
      }
    , { id = "fiber-baron"
      , name = "Fiber Baron"
      , description = "A million fibers! You're an empire."
      , check = TotalFiber 1000000
      }
    , { id = "fiber-tycoon"
      , name = "Fiber Tycoon"
      , description = "100 million fibers. Drowning in fluff."
      , check = TotalFiber 100000000
      }
    , { id = "fiber-legend"
      , name = "Fiber Legend"
      , description = "10 billion fibers. The world is your yarn ball."
      , check = TotalFiber 10000000000
      }

    -- ── Stitch milestones ──
    , { id = "first-stitch"
      , name = "First Stitch"
      , description = "Your first stitch. Every masterpiece starts here."
      , check = TotalStitches 1
      }
    , { id = "crafty"
      , name = "Crafty"
      , description = "100 stitches made."
      , check = TotalStitches 100
      }
    , { id = "stitch-wizard"
      , name = "Stitch Wizard"
      , description = "10,000 stitches! Pure magic."
      , check = TotalStitches 10000
      }
    , { id = "stitch-master"
      , name = "Stitch Master"
      , description = "A million stitches. Your fingers are legendary."
      , check = TotalStitches 1000000
      }
    , { id = "stitch-legend"
      , name = "Stitch Legend"
      , description = "100 million stitches. A tapestry of dedication."
      , check = TotalStitches 100000000
      }

    -- ── Click milestones ──
    , { id = "click-click"
      , name = "Click Click"
      , description = "10 clicks. Getting the hang of it."
      , check = TotalClicks 10
      }
    , { id = "clicker-pro"
      , name = "Clicker Pro"
      , description = "100 clicks. Dedicated gatherer!"
      , check = TotalClicks 100
      }
    , { id = "carpal-tunnel"
      , name = "Carpal Tunnel"
      , description = "1,000 clicks. Take a break maybe?"
      , check = TotalClicks 1000
      }
    , { id = "click-maniac"
      , name = "Click Maniac"
      , description = "10,000 clicks. Unstoppable!"
      , check = TotalClicks 10000
      }
    , { id = "click-legend"
      , name = "Click Legend"
      , description = "100,000 clicks. Your mouse fears you."
      , check = TotalClicks 100000
      }

    -- ── Sheep milestones ──
    , { id = "first-flock"
      , name = "First Flock"
      , description = "Buy your first sheep."
      , check = ProducerCount "sheep" 1
      }
    , { id = "shepherd"
      , name = "Shepherd"
      , description = "10 sheep! You're a proper shepherd."
      , check = ProducerCount "sheep" 10
      }
    , { id = "sheep-ranch"
      , name = "Sheep Ranch"
      , description = "25 sheep roaming the fields."
      , check = ProducerCount "sheep" 25
      }
    , { id = "sheep-empire"
      , name = "Sheep Empire"
      , description = "50 sheep! The hills are alive with bleating."
      , check = ProducerCount "sheep" 50
      }
    , { id = "sheep-legend"
      , name = "Sheep Legend"
      , description = "100 sheep. You are the wool whisperer."
      , check = ProducerCount "sheep" 100
      }

    -- ── Alpaca milestones ──
    , { id = "alpaca-friend"
      , name = "Alpaca Friend"
      , description = "Get your first alpaca."
      , check = ProducerCount "alpaca" 1
      }
    , { id = "alpaca-herd"
      , name = "Alpaca Herd"
      , description = "10 alpacas! So fluffy."
      , check = ProducerCount "alpaca" 10
      }
    , { id = "alpaca-ranch"
      , name = "Alpaca Ranch"
      , description = "25 alpacas grazing peacefully."
      , check = ProducerCount "alpaca" 25
      }
    , { id = "alpaca-empire"
      , name = "Alpaca Empire"
      , description = "50 alpacas. Peak fluffiness achieved."
      , check = ProducerCount "alpaca" 50
      }

    -- ── Spinning Wheel milestones ──
    , { id = "spinning-novice"
      , name = "Spinning Novice"
      , description = "Your first spinning wheel!"
      , check = ProducerCount "spinning-wheel" 1
      }
    , { id = "spinning-master"
      , name = "Spinning Master"
      , description = "10 spinning wheels whirring away."
      , check = ProducerCount "spinning-wheel" 10
      }
    , { id = "spinning-empire"
      , name = "Spinning Empire"
      , description = "25 wheels. A symphony of spinning."
      , check = ProducerCount "spinning-wheel" 25
      }

    -- ── Yarn Shop milestones ──
    , { id = "shop-owner"
      , name = "Shop Owner"
      , description = "Open your first yarn shop!"
      , check = ProducerCount "yarn-shop" 1
      }
    , { id = "shop-chain"
      , name = "Shop Chain"
      , description = "10 yarn shops across the land."
      , check = ProducerCount "yarn-shop" 10
      }
    , { id = "yarn-mogul"
      , name = "Yarn Mogul"
      , description = "25 shops. Yarn on every corner."
      , check = ProducerCount "yarn-shop" 25
      }

    -- ── Fiber Farm milestones ──
    , { id = "farm-founder"
      , name = "Farm Founder"
      , description = "Your first fiber farm!"
      , check = ProducerCount "fiber-farm" 1
      }
    , { id = "farm-tycoon"
      , name = "Farm Tycoon"
      , description = "10 fiber farms. Rolling green pastures."
      , check = ProducerCount "fiber-farm" 10
      }
    , { id = "farm-empire"
      , name = "Farm Empire"
      , description = "25 farms. A countryside empire."
      , check = ProducerCount "fiber-farm" 25
      }

    -- ── Fiber Mill milestones ──
    , { id = "mill-builder"
      , name = "Mill Builder"
      , description = "Build your first fiber mill!"
      , check = ProducerCount "fiber-mill" 1
      }
    , { id = "mill-tycoon"
      , name = "Mill Tycoon"
      , description = "10 mills humming with production."
      , check = ProducerCount "fiber-mill" 10
      }
    , { id = "mill-empire"
      , name = "Mill Empire"
      , description = "25 mills. Industrial fiber dominance."
      , check = ProducerCount "fiber-mill" 25
      }

    -- ── Diversification ──
    , { id = "diversified"
      , name = "Diversified"
      , description = "Own at least 3 different types of producers."
      , check = TotalProducerTypes 3
      }
    , { id = "full-barn"
      , name = "Full Barn"
      , description = "Own every type of producer!"
      , check = TotalProducerTypes 6
      }

    -- ── Project milestones ──
    , { id = "first-project"
      , name = "First Project"
      , description = "Start your first craft project."
      , check = TotalProjectTypes 1
      }
    , { id = "pattern-collector"
      , name = "Pattern Collector"
      , description = "Have 3 different types of projects."
      , check = TotalProjectTypes 3
      }
    , { id = "master-crafter"
      , name = "Master Crafter"
      , description = "Own every type of craft project!"
      , check = TotalProjectTypes 6
      }

    -- ── Granny Square milestones ──
    , { id = "granny-crafter"
      , name = "Granny Crafter"
      , description = "10 granny squares! A colorful collection."
      , check = ProjectCount "granny-square" 10
      }
    , { id = "granny-master"
      , name = "Granny Master"
      , description = "50 granny squares. Time for a blanket?"
      , check = ProjectCount "granny-square" 50
      }

    -- ── Scarf milestones ──
    , { id = "scarf-knitter"
      , name = "Scarf Knitter"
      , description = "10 scarves. Everyone stays warm!"
      , check = ProjectCount "scarf" 10
      }
    , { id = "scarf-master"
      , name = "Scarf Master"
      , description = "50 scarves! A mountain of coziness."
      , check = ProjectCount "scarf" 50
      }

    -- ── Beanie milestones ──
    , { id = "beanie-maker"
      , name = "Beanie Maker"
      , description = "10 beanies. Hats for everyone!"
      , check = ProjectCount "beanie" 10
      }
    , { id = "beanie-master"
      , name = "Beanie Master"
      , description = "25 beanies. A hat for every head."
      , check = ProjectCount "beanie" 25
      }

    -- ── Sweater milestones ──
    , { id = "sweater-knitter"
      , name = "Sweater Knitter"
      , description = "10 sweaters. Chunky knit heaven."
      , check = ProjectCount "sweater" 10
      }
    , { id = "sweater-master"
      , name = "Sweater Master"
      , description = "25 sweaters. Fashion icon!"
      , check = ProjectCount "sweater" 25
      }

    -- ── Blanket milestones ──
    , { id = "blanket-crafter"
      , name = "Blanket Crafter"
      , description = "10 blankets. Maximum coziness."
      , check = ProjectCount "blanket" 10
      }
    , { id = "blanket-master"
      , name = "Blanket Master"
      , description = "25 blankets. The whole town is snug."
      , check = ProjectCount "blanket" 25
      }

    -- ── Tapestry milestones ──
    , { id = "tapestry-weaver"
      , name = "Tapestry Weaver"
      , description = "Your first tapestry. A work of art!"
      , check = ProjectCount "tapestry" 1
      }
    , { id = "tapestry-artisan"
      , name = "Tapestry Artisan"
      , description = "10 tapestries. Your gallery grows."
      , check = ProjectCount "tapestry" 10
      }
    , { id = "tapestry-master"
      , name = "Tapestry Master"
      , description = "25 tapestries. A true artisan."
      , check = ProjectCount "tapestry" 25
      }
    , { id = "tapestry-legend"
      , name = "Tapestry Legend"
      , description = "50 tapestries! Museums call for your work."
      , check = ProjectCount "tapestry" 50
      }
    , { id = "tapestry-empire"
      , name = "Tapestry Empire"
      , description = "100 tapestries. An unmatched legacy of woven art."
      , check = ProjectCount "tapestry" 100
      }
    ]


{-| Check if an achievement's condition is met.
-}
isUnlocked : CheckType -> GameStats -> Bool
isUnlocked check stats =
    case check of
        TotalFiber amount ->
            stats.totalFiberEarned >= amount

        TotalStitches amount ->
            stats.totalStitchesEarned >= amount

        TotalClicks count ->
            stats.totalClicks >= count

        ProducerCount producerId count ->
            (Dict.get producerId stats.producers |> Maybe.withDefault 0) >= count

        ProjectCount projectId count ->
            (Dict.get projectId stats.projects |> Maybe.withDefault 0) >= count

        TotalProducerTypes count ->
            let
                ownedTypes =
                    Dict.values stats.producers
                        |> List.filter (\n -> n > 0)
                        |> List.length
            in
            ownedTypes >= count

        TotalProjectTypes count ->
            let
                ownedTypes =
                    Dict.values stats.projects
                        |> List.filter (\n -> n > 0)
                        |> List.length
            in
            ownedTypes >= count


{-| Calculate the production bonus from unlocked achievements.
    Each achievement gives +1% to all production (was 2%, tuned down with more achievements).
-}
productionBonus : Set String -> Float
productionBonus unlockedIds =
    1.0 + (toFloat (Set.size unlockedIds) * 0.01)


{-| A cozy hint about what an achievement requires, without revealing exact details.
-}
hintFor : CheckType -> String
hintFor check =
    case check of
        TotalFiber _ ->
            "Gather more fiber..."

        TotalStitches _ ->
            "Keep crafting stitches..."

        TotalClicks _ ->
            "Keep clicking!"

        ProducerCount id _ ->
            "Get more " ++ producerDisplayName id ++ "..."

        ProjectCount id _ ->
            "Craft more " ++ projectDisplayName id ++ "..."

        TotalProducerTypes _ ->
            "Diversify your producers..."

        TotalProjectTypes _ ->
            "Try new craft projects..."


{-| Compute progress towards an achievement.
-}
progressFor : CheckType -> GameStats -> Progress
progressFor check stats =
    case check of
        TotalFiber target ->
            { current = Basics.min stats.totalFiberEarned target, target = target }

        TotalStitches target ->
            { current = Basics.min stats.totalStitchesEarned target, target = target }

        TotalClicks target ->
            { current = Basics.min (toFloat stats.totalClicks) (toFloat target), target = toFloat target }

        ProducerCount id target ->
            let
                owned =
                    toFloat (Dict.get id stats.producers |> Maybe.withDefault 0)
            in
            { current = Basics.min owned (toFloat target), target = toFloat target }

        ProjectCount id target ->
            let
                owned =
                    toFloat (Dict.get id stats.projects |> Maybe.withDefault 0)
            in
            { current = Basics.min owned (toFloat target), target = toFloat target }

        TotalProducerTypes target ->
            let
                ownedTypes =
                    toFloat (Dict.values stats.producers |> List.filter (\n -> n > 0) |> List.length)
            in
            { current = Basics.min ownedTypes (toFloat target), target = toFloat target }

        TotalProjectTypes target ->
            let
                ownedTypes =
                    toFloat (Dict.values stats.projects |> List.filter (\n -> n > 0) |> List.length)
            in
            { current = Basics.min ownedTypes (toFloat target), target = toFloat target }


{-| Friendly display name for a producer id.
-}
producerDisplayName : String -> String
producerDisplayName id =
    case id of
        "sheep" ->
            "sheep"

        "alpaca" ->
            "alpacas"

        "spinning-wheel" ->
            "spinning wheels"

        "yarn-shop" ->
            "yarn shops"

        "fiber-farm" ->
            "fiber farms"

        "fiber-mill" ->
            "fiber mills"

        _ ->
            id


{-| Friendly display name for a project id.
-}
projectDisplayName : String -> String
projectDisplayName id =
    case id of
        "granny-square" ->
            "granny squares"

        "scarf" ->
            "scarves"

        "beanie" ->
            "beanies"

        "sweater" ->
            "sweaters"

        "blanket" ->
            "blankets"

        "tapestry" ->
            "tapestries"

        _ ->
            id
