module Pages.Home_ exposing (Model, Msg, page)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Json.Decode as Decode
import Json.Encode as Encode
import Lib.Achievements as Achievements
import Lib.Format as Format
import Lib.GameState as GameState exposing (GameState)
import Lib.Producers as Producers
import Lib.Projects as Projects
import Lib.Upgrades as Upgrades
import Page exposing (Page)
import Ports
import Set
import Task
import Time
import View exposing (View)


page : Page Model Msg
page =
    Page.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { game : GameState
    , tickCount : Int
    , clickParticles : List { id : Int, age : Int }
    , nextParticleId : Int
    , selectedAchievement : Maybe String
    , showResetConfirm : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { game = GameState.init
      , tickCount = 0
      , clickParticles = []
      , nextParticleId = 0
      , selectedAchievement = Nothing
      , showResetConfirm = False
      }
    , Task.perform Tick Time.now
    )



-- UPDATE


type Msg
    = Click
    | Tick Time.Posix
    | BuyProducer String
    | StartProject String
    | BuyUpgrade String
    | LoadedGame Encode.Value
    | SelectAchievement String
    | CloseAchievementDetail
    | ResetGame
    | CancelReset
    | ConfirmReset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click ->
            ( { model
                | game = GameState.click model.game |> GameState.checkAchievements
                , clickParticles = { id = model.nextParticleId, age = 0 } :: model.clickParticles
                , nextParticleId = model.nextParticleId + 1
              }
            , Cmd.none
            )

        Tick time ->
            let
                newGame =
                    GameState.tick time model.game |> GameState.checkAchievements

                newTickCount =
                    model.tickCount + 1

                saveCmd =
                    if modBy 300 newTickCount == 0 then
                        Ports.saveGame (GameState.encode newGame)

                    else
                        Cmd.none

                updatedParticles =
                    model.clickParticles
                        |> List.map (\p -> { p | age = p.age + 1 })
                        |> List.filter (\p -> p.age <= 10)
            in
            ( { model | game = newGame, tickCount = newTickCount, clickParticles = updatedParticles }
            , saveCmd
            )

        BuyProducer producerId ->
            ( { model | game = GameState.buyProducer producerId model.game |> GameState.checkAchievements }
            , Cmd.none
            )

        StartProject projectId ->
            ( { model | game = GameState.startProject projectId model.game |> GameState.checkAchievements }
            , Cmd.none
            )

        BuyUpgrade upgradeId ->
            ( { model | game = GameState.buyUpgrade upgradeId model.game |> GameState.checkAchievements }
            , Cmd.none
            )

        LoadedGame value ->
            case Decode.decodeValue GameState.decoder value of
                Ok loadedState ->
                    ( { model | game = { loadedState | lastTick = Time.millisToPosix 0 } }
                    , Task.perform Tick Time.now
                    )

                Err _ ->
                    ( model, Cmd.none )

        SelectAchievement achievementId ->
            let
                newSelected =
                    if model.selectedAchievement == Just achievementId then
                        Nothing

                    else
                        Just achievementId
            in
            ( { model | selectedAchievement = newSelected }, Cmd.none )

        CloseAchievementDetail ->
            ( { model | selectedAchievement = Nothing }, Cmd.none )

        ResetGame ->
            ( { model | showResetConfirm = True }, Cmd.none )

        CancelReset ->
            ( { model | showResetConfirm = False }, Cmd.none )

        ConfirmReset ->
            ( { model
                | game = GameState.init
                , tickCount = 0
                , clickParticles = []
                , nextParticleId = 0
                , selectedAchievement = Nothing
                , showResetConfirm = False
              }
            , Ports.clearSave ()
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 100 Tick
        , Ports.loadedGame LoadedGame
        ]



-- VIEW


view : Model -> View Msg
view model =
    { title = "Fiber Empire"
    , body =
        [ div [ class "app" ]
            [ viewHeader model
            , div [ class "main" ]
                [ viewProducersPanel model.game
                , viewCenter model
                , viewProjectsPanel model.game
                ]
            , viewUpgradesBar model.game
            , viewAchievementsBar model
            , viewAchievementPopup model
            ]
        ]
    }


viewHeader : Model -> Html Msg
viewHeader model =
    let
        game =
            model.game
    in
    div [ class "header" ]
        [ h1 [ class "title" ]
            [ img [ src "/images/yarn-ball.png", class "title-icon", Html.Attributes.alt "Fiber Empire" ] []
            , text "Fiber Empire"
            ]
        , div [ class "resources" ]
            [ div [ class "resource" ]
                [ img [ src "/images/yarn-ball.png", class "resource-icon-img", Html.Attributes.alt "fiber" ] []
                , span [ class "resource-label" ] [ text "Fiber" ]
                , span [ class "resource-value" ] [ text (Format.formatNumber game.fiber) ]
                , span [ class "resource-rate" ] [ text ("+" ++ Format.formatRate (GameState.fiberPerSecond game)) ]
                ]
            , div [ class "resource" ]
                [ span [ class "resource-icon" ] [ text "\u{1FAA1}" ]
                , span [ class "resource-label" ] [ text "Stitches" ]
                , span [ class "resource-value" ] [ text (Format.formatNumber game.stitches) ]
                , span [ class "resource-rate" ] [ text ("+" ++ Format.formatRate (GameState.stitchesPerSecond game)) ]
                ]
            ]
        , button [ class "reset-btn", onClick ResetGame ] [ text "\u{21BB}" ]
        , viewResetConfirm model.showResetConfirm
        ]


viewResetConfirm : Bool -> Html Msg
viewResetConfirm show =
    if show then
        div [ class "reset-overlay" ]
            [ div [ class "reset-overlay-backdrop", onClick CancelReset ] []
            , div [ class "reset-popup" ]
                [ p [ class "reset-title" ] [ text "Reset Game?" ]
                , p [ class "reset-text" ] [ text "All progress will be lost. This cannot be undone!" ]
                , div [ class "reset-actions" ]
                    [ button [ class "reset-cancel", onClick CancelReset ] [ text "Keep Playing" ]
                    , button [ class "reset-confirm", onClick ConfirmReset ] [ text "Reset Everything" ]
                    ]
                ]
            ]

    else
        text ""


viewCenter : Model -> Html Msg
viewCenter model =
    let
        game =
            model.game

        producerItems =
            buildOrbitItems game.producers Producers.allProducers

        projectItems =
            buildOrbitItems game.projects Projects.allProjects

        totalInner =
            List.length producerItems

        totalOuter =
            List.length projectItems
    in
    div [ class "center-panel" ]
        [ div [ class "orbit-area" ]
            [ if totalOuter > 0 then
                div [ class "orbit-ring orbit-outer" ]
                    (List.indexedMap (viewOrbitSlot totalOuter) projectItems)

              else
                text ""
            , if totalInner > 0 then
                div [ class "orbit-ring orbit-inner" ]
                    (List.indexedMap (viewOrbitSlot totalInner) producerItems)

              else
                text ""
            , div [ class "yarn-ball-wrapper" ]
                [ button [ class "yarn-ball", onClick Click ]
                    [ img [ src "/images/yarn-ball.png", class "yarn-ball-img", Html.Attributes.alt "Gather Fiber" ] []
                    , span [ class "yarn-ball-label" ] [ text "Gather Fiber!" ]
                    ]
                , Keyed.node "div"
                    [ class "click-particles" ]
                    (List.map (\p -> ( String.fromInt p.id, viewClickParticle game p )) model.clickParticles)
                ]
            ]
        , div [ class "click-info" ]
            [ text ("+" ++ Format.formatNumber (GameState.fiberPerClick game) ++ " per click") ]
        , viewStats game
        ]


{-| Build a list of orbit items.
    Per type: 1-3 owned -> show individual icons (lively early feel).
    4+ owned -> collapse to one icon with a count badge.
-}
buildOrbitItems : Dict.Dict String Int -> List { a | id : String, image : String } -> List { image : String, count : Int }
buildOrbitItems owned items =
    items
        |> List.concatMap
            (\item ->
                let
                    count =
                        Dict.get item.id owned |> Maybe.withDefault 0
                in
                if count <= 0 then
                    []

                else if count <= 3 then
                    -- Show individual icons, no badge
                    List.repeat count { image = item.image, count = 0 }

                else
                    -- Collapse to one icon with badge
                    [ { image = item.image, count = count } ]
            )


viewOrbitSlot : Int -> Int -> { image : String, count : Int } -> Html msg
viewOrbitSlot total index item =
    let
        hasBadge =
            item.count > 1

        slotClass =
            if hasBadge then
                "orbit-slot has-badge"

            else
                "orbit-slot"
    in
    div
        [ class slotClass
        , attribute "style" ("--i: " ++ String.fromInt index ++ "; --total: " ++ String.fromInt total)
        ]
        [ div [ class "orbit-icon-wrapper" ]
            (img [ src item.image, class "orbit-icon" ] []
                :: (if hasBadge then
                        [ span [ class "orbit-count" ] [ text ("x" ++ String.fromInt item.count) ] ]

                    else
                        []
                   )
            )
        ]


viewClickParticle : GameState -> { id : Int, age : Int } -> Html msg
viewClickParticle game particle =
    let
        xOffset =
            modBy 80 (particle.id * 37 + 13) - 40
    in
    span
        [ class "click-particle"
        , style "left" ("calc(50% + " ++ String.fromInt xOffset ++ "px)")
        ]
        [ text ("+" ++ Format.formatNumber (GameState.fiberPerClick game)) ]


viewStats : GameState -> Html Msg
viewStats game =
    div [ class "stats" ]
        [ h3 [] [ text "Stats" ]
        , div [ class "stat-row" ]
            [ span [] [ text "Total fiber gathered:" ]
            , span [] [ text (Format.formatNumber game.totalFiberEarned) ]
            ]
        , div [ class "stat-row" ]
            [ span [] [ text "Total stitches made:" ]
            , span [] [ text (Format.formatNumber game.totalStitchesEarned) ]
            ]
        , div [ class "stat-row" ]
            [ span [] [ text "Total clicks:" ]
            , span [] [ text (String.fromInt game.totalClicks) ]
            ]
        , div [ class "stat-row" ]
            [ span [] [ text "Achievements:" ]
            , span [] [ text (String.fromInt (Set.size game.achievements) ++ "/" ++ String.fromInt (List.length Achievements.allAchievements)) ]
            ]
        ]


viewProducersPanel : GameState -> Html Msg
viewProducersPanel game =
    div [ class "panel producers-panel" ]
        [ h2 [ class "panel-title" ] [ text "Producers" ]
        , p [ class "panel-subtitle" ] [ text "Generate fiber automatically" ]
        , div [ class "item-list" ]
            (List.map (viewProducerCard game) Producers.allProducers)
        ]


viewProducerCard : GameState -> Producers.Producer -> Html Msg
viewProducerCard game producer =
    let
        owned =
            game.producers |> Dict.get producer.id |> Maybe.withDefault 0

        cost =
            GameState.producerCost producer.id game

        affordable =
            GameState.canAffordProducer producer.id game
    in
    div
        [ class "item-card"
        , classList [ ( "affordable", affordable ) ]
        , onClick (BuyProducer producer.id)
        ]
        [ div [ class "item-card-body" ]
            [ img [ src producer.image, class "item-icon", Html.Attributes.alt producer.name ] []
            , div [ class "item-info" ]
                [ div [ class "item-header" ]
                    [ span [ class "item-name" ] [ text producer.name ]
                    , span [ class "item-count" ] [ text ("x" ++ String.fromInt owned) ]
                    ]
                , div [ class "item-description" ] [ text producer.description ]
                , div [ class "item-footer" ]
                    [ span [ class "item-cost" ]
                        [ text ("\u{1F9F6} " ++ Format.formatNumber cost) ]
                    , span [ class "item-output" ]
                        [ text ("+" ++ Format.formatNumber producer.baseOutput ++ "/s each") ]
                    ]
                ]
            ]
        ]


viewProjectsPanel : GameState -> Html Msg
viewProjectsPanel game =
    div [ class "panel projects-panel" ]
        [ h2 [ class "panel-title" ] [ text "Projects" ]
        , p [ class "panel-subtitle" ] [ text "Craft items to earn stitches" ]
        , div [ class "item-list" ]
            (List.map (viewProjectCard game) Projects.allProjects)
        ]


viewProjectCard : GameState -> Projects.Project -> Html Msg
viewProjectCard game project =
    let
        owned =
            game.projects |> Dict.get project.id |> Maybe.withDefault 0

        cost =
            GameState.projectCost project.id game

        affordable =
            GameState.canAffordProject project.id game
    in
    div
        [ class "item-card"
        , classList [ ( "affordable", affordable ) ]
        , onClick (StartProject project.id)
        ]
        [ div [ class "item-card-body" ]
            [ img [ src project.image, class "item-icon", Html.Attributes.alt project.name ] []
            , div [ class "item-info" ]
                [ div [ class "item-header" ]
                    [ span [ class "item-name" ] [ text project.name ]
                    , span [ class "item-count" ] [ text ("x" ++ String.fromInt owned) ]
                    ]
                , div [ class "item-description" ] [ text project.description ]
                , div [ class "item-footer" ]
                    [ span [ class "item-cost" ]
                        [ text ("\u{1F9F6} " ++ Format.formatNumber cost) ]
                    , span [ class "item-output" ]
                        [ text ("+" ++ Format.formatNumber project.baseOutput ++ " stitches/s") ]
                    ]
                ]
            ]
        ]


viewUpgradesBar : GameState -> Html Msg
viewUpgradesBar game =
    let
        availableUpgrades =
            Upgrades.allUpgrades
                |> List.filter (\u -> not (Set.member u.id game.upgrades))
    in
    if List.isEmpty availableUpgrades then
        text ""

    else
        div [ class "upgrades-bar" ]
            [ h2 [ class "bar-title" ] [ text "Upgrades" ]
            , div [ class "upgrades-list" ]
                (List.map (viewUpgradeCard game) availableUpgrades)
            ]


viewUpgradeCard : GameState -> Upgrades.Upgrade -> Html Msg
viewUpgradeCard game upgrade_ =
    let
        affordable =
            GameState.canAffordUpgrade upgrade_.id game
    in
    div
        [ class "upgrade-card"
        , classList [ ( "affordable", affordable ) ]
        , onClick (BuyUpgrade upgrade_.id)
        ]
        [ div [ class "upgrade-name" ] [ text upgrade_.name ]
        , div [ class "upgrade-description" ] [ text upgrade_.description ]
        , div [ class "upgrade-cost" ]
            [ text ("\u{1FAA1} " ++ Format.formatNumber upgrade_.cost) ]
        ]


viewAchievementsBar : Model -> Html Msg
viewAchievementsBar model =
    let
        game =
            model.game

        unlockedCount =
            Set.size game.achievements

        totalCount =
            List.length Achievements.allAchievements
    in
    div [ class "achievements-bar" ]
        [ h2 [ class "bar-title" ]
            [ text "Achievements"
            , span [ class "achievements-counter" ]
                [ text (" " ++ String.fromInt unlockedCount ++ "/" ++ String.fromInt totalCount) ]
            ]
        , div [ class "achievements-list" ]
            (List.map (viewAchievement game model.selectedAchievement) Achievements.allAchievements)
        ]


viewAchievementPopup : Model -> Html Msg
viewAchievementPopup model =
    case model.selectedAchievement of
        Nothing ->
            text ""

        Just selId ->
            case
                Achievements.allAchievements
                    |> List.filter (\a -> a.id == selId)
                    |> List.head
            of
                Nothing ->
                    text ""

                Just achievement ->
                    let
                        game =
                            model.game

                        stats =
                            GameState.toStats game
                    in
                    div [ class "achievement-overlay" ]
                        [ div [ class "achievement-overlay-backdrop", onClick CloseAchievementDetail ] []
                        , div [ class "achievement-popup" ]
                            [ viewAchievementDetail game stats achievement ]
                        ]


viewAchievement : GameState -> Maybe String -> Achievements.Achievement -> Html Msg
viewAchievement game selected achievement =
    let
        unlocked =
            Set.member achievement.id game.achievements

        isSelected =
            selected == Just achievement.id
    in
    div
        [ class "achievement"
        , classList
            [ ( "unlocked", unlocked )
            , ( "locked", not unlocked )
            , ( "selected", isSelected )
            ]
        , onClick (SelectAchievement achievement.id)
        ]
        [ span [ class "achievement-icon" ]
            [ text
                (if unlocked then
                    "\u{2B50}"

                 else
                    "\u{1F512}"
                )
            ]
        , if unlocked then
            span [ class "achievement-name" ] [ text achievement.name ]

          else
            span [ class "achievement-name" ] [ text "???" ]
        ]


viewAchievementDetail : GameState -> Achievements.GameStats -> Achievements.Achievement -> Html Msg
viewAchievementDetail game stats achievement =
    let
        unlocked =
            Set.member achievement.id game.achievements

        progress =
            Achievements.progressFor achievement.check stats

        progressPct =
            if progress.target > 0 then
                Basics.min 100 (progress.current / progress.target * 100)

            else
                0
    in
    div [ class "achievement-detail" ]
        [ button [ class "achievement-detail-close", onClick CloseAchievementDetail ] [ text "\u{00D7}" ]
        , div [ class "achievement-detail-icon" ]
            [ text
                (if unlocked then
                    "\u{2B50}"

                 else
                    "\u{1F512}"
                )
            ]
        , div [ class "achievement-detail-name" ]
            [ text
                (if unlocked then
                    achievement.name

                 else
                    "???"
                )
            ]
        , div [ class "achievement-detail-desc" ]
            [ text
                (if unlocked then
                    achievement.description

                 else
                    Achievements.hintFor achievement.check
                )
            ]
        , if not unlocked then
            div [ class "achievement-detail-progress" ]
                [ div [ class "progress-bar" ]
                    [ div
                        [ class "progress-fill"
                        , style "width" (String.fromFloat progressPct ++ "%")
                        ]
                        []
                    ]
                , div [ class "progress-label" ]
                    [ text (Format.formatNumber progress.current ++ " / " ++ Format.formatNumber progress.target) ]
                ]

          else
            div [ class "achievement-detail-unlocked" ] [ text "Achieved!" ]
        ]
