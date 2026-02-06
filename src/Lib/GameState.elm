module Lib.GameState exposing
    ( GameState
    , buyProducer
    , buyUpgrade
    , canAffordProducer
    , canAffordProject
    , canAffordUpgrade
    , checkAchievements
    , click
    , decoder
    , encode
    , fiberPerClick
    , fiberPerSecond
    , init
    , producerCost
    , projectCost
    , startProject
    , stitchesPerSecond
    , tick
    , toStats
    )

{-| Core game state and logic for the Fiber Empire game.
-}

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Lib.Achievements as Achievements
import Lib.Producers as Producers
import Lib.Projects as Projects
import Lib.Upgrades as Upgrades
import Set exposing (Set)
import Time


type alias GameState =
    { fiber : Float
    , stitches : Float
    , producers : Dict String Int
    , projects : Dict String Int
    , upgrades : Set String
    , achievements : Set String
    , totalFiberEarned : Float
    , totalStitchesEarned : Float
    , totalClicks : Int
    , lastTick : Time.Posix
    }


{-| Initial game state with zero resources and no items.
-}
init : GameState
init =
    { fiber = 0
    , stitches = 0
    , producers = Dict.empty
    , projects = Dict.empty
    , upgrades = Set.empty
    , achievements = Set.empty
    , totalFiberEarned = 0
    , totalStitchesEarned = 0
    , totalClicks = 0
    , lastTick = Time.millisToPosix 0
    }



-- CALCULATIONS


{-| Effective fiber gained per click, accounting for upgrades and achievements.
-}
fiberPerClick : GameState -> Float
fiberPerClick state =
    let
        base =
            1.0

        upgradeMultiplier =
            Upgrades.fiberClickMultiplier state.upgrades

        achievementMultiplier =
            Achievements.productionBonus state.achievements
    in
    base * upgradeMultiplier * achievementMultiplier


{-| Total fiber generated per second from all producers, with multipliers.
-}
fiberPerSecond : GameState -> Float
fiberPerSecond state =
    let
        baseOutput =
            Producers.allProducers
                |> List.map
                    (\p ->
                        let
                            count =
                                Dict.get p.id state.producers |> Maybe.withDefault 0
                        in
                        Producers.outputPerSecond p count
                    )
                |> List.sum

        upgradeMultiplier =
            Upgrades.fiberProductionMultiplier state.upgrades

        achievementMultiplier =
            Achievements.productionBonus state.achievements
    in
    baseOutput * upgradeMultiplier * achievementMultiplier


{-| Total stitches generated per second from all projects, with multipliers.
-}
stitchesPerSecond : GameState -> Float
stitchesPerSecond state =
    let
        baseOutput =
            Projects.allProjects
                |> List.map
                    (\p ->
                        let
                            count =
                                Dict.get p.id state.projects |> Maybe.withDefault 0
                        in
                        Projects.outputPerSecond p count
                    )
                |> List.sum

        upgradeMultiplier =
            Upgrades.stitchProductionMultiplier state.upgrades

        achievementMultiplier =
            Achievements.productionBonus state.achievements
    in
    baseOutput * upgradeMultiplier * achievementMultiplier



-- ACTIONS


{-| Process a click action: add fiber, increment click count.
-}
click : GameState -> GameState
click state =
    let
        gained =
            fiberPerClick state
    in
    { state
        | fiber = state.fiber + gained
        , totalFiberEarned = state.totalFiberEarned + gained
        , totalClicks = state.totalClicks + 1
    }


{-| Process a game tick: add passive production based on elapsed time.
    If lastTick is epoch (0), just set the time without producing.
-}
tick : Time.Posix -> GameState -> GameState
tick now state =
    if Time.posixToMillis state.lastTick == 0 then
        { state | lastTick = now }

    else
        let
            deltaMs =
                Time.posixToMillis now - Time.posixToMillis state.lastTick

            deltaSec =
                toFloat deltaMs / 1000.0

            fiberGained =
                fiberPerSecond state * deltaSec

            stitchesGained =
                stitchesPerSecond state * deltaSec
        in
        { state
            | fiber = state.fiber + fiberGained
            , stitches = state.stitches + stitchesGained
            , totalFiberEarned = state.totalFiberEarned + fiberGained
            , totalStitchesEarned = state.totalStitchesEarned + stitchesGained
            , lastTick = now
        }


{-| Buy a producer if the player can afford it.
-}
buyProducer : String -> GameState -> GameState
buyProducer producerId state =
    case Producers.findById producerId of
        Nothing ->
            state

        Just producer ->
            let
                owned =
                    Dict.get producerId state.producers |> Maybe.withDefault 0

                cost =
                    Producers.currentCost producer owned
            in
            if state.fiber >= cost then
                { state
                    | fiber = state.fiber - cost
                    , producers = Dict.insert producerId (owned + 1) state.producers
                }

            else
                state


{-| Start a craft project if the player can afford it.
-}
startProject : String -> GameState -> GameState
startProject projectId state =
    case Projects.findById projectId of
        Nothing ->
            state

        Just project ->
            let
                owned =
                    Dict.get projectId state.projects |> Maybe.withDefault 0

                cost =
                    Projects.currentCost project owned
            in
            if state.fiber >= cost then
                { state
                    | fiber = state.fiber - cost
                    , projects = Dict.insert projectId (owned + 1) state.projects
                }

            else
                state


{-| Buy an upgrade if the player can afford it and doesn't already own it.
-}
buyUpgrade : String -> GameState -> GameState
buyUpgrade upgradeId state =
    if Set.member upgradeId state.upgrades then
        state

    else
        case Upgrades.findById upgradeId of
            Nothing ->
                state

            Just upgrade ->
                if state.stitches >= upgrade.cost then
                    { state
                        | stitches = state.stitches - upgrade.cost
                        , upgrades = Set.insert upgradeId state.upgrades
                    }

                else
                    state



-- AFFORDABILITY CHECKS


producerCost : String -> GameState -> Float
producerCost producerId state =
    case Producers.findById producerId of
        Nothing ->
            0

        Just producer ->
            let
                owned =
                    Dict.get producerId state.producers |> Maybe.withDefault 0
            in
            Producers.currentCost producer owned


projectCost : String -> GameState -> Float
projectCost projectId state =
    case Projects.findById projectId of
        Nothing ->
            0

        Just project ->
            let
                owned =
                    Dict.get projectId state.projects |> Maybe.withDefault 0
            in
            Projects.currentCost project owned


canAffordProducer : String -> GameState -> Bool
canAffordProducer producerId state =
    state.fiber >= producerCost producerId state


canAffordProject : String -> GameState -> Bool
canAffordProject projectId state =
    state.fiber >= projectCost projectId state


canAffordUpgrade : String -> GameState -> Bool
canAffordUpgrade upgradeId state =
    case Upgrades.findById upgradeId of
        Nothing ->
            False

        Just upgrade ->
            not (Set.member upgradeId state.upgrades)
                && (state.stitches >= upgrade.cost)



-- ACHIEVEMENTS


{-| Convert game state to the stats record needed by the Achievements module.
-}
toStats : GameState -> Achievements.GameStats
toStats state =
    { totalFiberEarned = state.totalFiberEarned
    , totalStitchesEarned = state.totalStitchesEarned
    , totalClicks = state.totalClicks
    , producers = state.producers
    , projects = state.projects
    }


{-| Check all achievements and unlock any newly earned ones.
-}
checkAchievements : GameState -> GameState
checkAchievements state =
    let
        stats =
            toStats state

        newlyUnlocked =
            Achievements.allAchievements
                |> List.filter (\a -> not (Set.member a.id state.achievements))
                |> List.filter (\a -> Achievements.isUnlocked a.check stats)
                |> List.map .id

        newAchievements =
            List.foldl Set.insert state.achievements newlyUnlocked
    in
    { state | achievements = newAchievements }



-- JSON ENCODING / DECODING


{-| Encode game state to JSON for saving to localStorage.
-}
encode : GameState -> Encode.Value
encode state =
    Encode.object
        [ ( "fiber", Encode.float state.fiber )
        , ( "stitches", Encode.float state.stitches )
        , ( "producers", encodeStringDict Encode.int state.producers )
        , ( "projects", encodeStringDict Encode.int state.projects )
        , ( "upgrades", Encode.list Encode.string (Set.toList state.upgrades) )
        , ( "achievements", Encode.list Encode.string (Set.toList state.achievements) )
        , ( "totalFiberEarned", Encode.float state.totalFiberEarned )
        , ( "totalStitchesEarned", Encode.float state.totalStitchesEarned )
        , ( "totalClicks", Encode.int state.totalClicks )
        , ( "lastTick", Encode.int (Time.posixToMillis state.lastTick) )
        ]


encodeStringDict : (v -> Encode.Value) -> Dict String v -> Encode.Value
encodeStringDict encodeValue dict =
    dict
        |> Dict.toList
        |> List.map (\( k, v ) -> ( k, encodeValue v ))
        |> Encode.object


{-| Decode game state from JSON (loaded from localStorage).
-}
decoder : Decode.Decoder GameState
decoder =
    Decode.succeed GameState
        |> andMap (Decode.field "fiber" Decode.float)
        |> andMap (Decode.field "stitches" Decode.float)
        |> andMap (Decode.field "producers" (Decode.dict Decode.int))
        |> andMap (Decode.field "projects" (Decode.dict Decode.int))
        |> andMap (Decode.field "upgrades" (Decode.list Decode.string |> Decode.map Set.fromList))
        |> andMap (Decode.field "achievements" (Decode.list Decode.string |> Decode.map Set.fromList))
        |> andMap (Decode.field "totalFiberEarned" Decode.float)
        |> andMap (Decode.field "totalStitchesEarned" Decode.float)
        |> andMap (Decode.field "totalClicks" Decode.int)
        |> andMap (Decode.field "lastTick" (Decode.int |> Decode.map Time.millisToPosix))


andMap : Decode.Decoder a -> Decode.Decoder (a -> b) -> Decode.Decoder b
andMap =
    Decode.map2 (|>)
