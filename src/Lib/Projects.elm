module Lib.Projects exposing
    ( Project
    , allProjects
    , currentCost
    , findById
    , outputPerSecond
    )

{-| Craft Project definitions for the Fiber Empire game.
    Projects cost Fiber to start and generate Stitches passively.
-}


type alias Project =
    { id : String
    , name : String
    , description : String
    , image : String
    , baseCost : Float
    , baseOutput : Float
    , costMultiplier : Float
    }


allProjects : List Project
allProjects =
    [ { id = "granny-square"
      , name = "Granny Square"
      , description = "A simple crochet square."
      , image = "/images/granny-square.png"
      , baseCost = 50
      , baseOutput = 0.2
      , costMultiplier = 1.15
      }
    , { id = "scarf"
      , name = "Scarf"
      , description = "A warm, cozy scarf."
      , image = "/images/scarf.png"
      , baseCost = 300
      , baseOutput = 1
      , costMultiplier = 1.15
      }
    , { id = "beanie"
      , name = "Beanie"
      , description = "A cute knitted hat."
      , image = "/images/beanie.png"
      , baseCost = 1500
      , baseOutput = 4
      , costMultiplier = 1.15
      }
    , { id = "sweater"
      , name = "Sweater"
      , description = "A chunky knit pullover."
      , image = "/images/sweater.png"
      , baseCost = 10000
      , baseOutput = 20
      , costMultiplier = 1.15
      }
    , { id = "blanket"
      , name = "Blanket"
      , description = "A massive crochet blanket."
      , image = "/images/blanket.png"
      , baseCost = 75000
      , baseOutput = 100
      , costMultiplier = 1.15
      }
    , { id = "tapestry"
      , name = "Tapestry"
      , description = "A woven masterpiece."
      , image = "/images/tapestry.png"
      , baseCost = 500000
      , baseOutput = 500
      , costMultiplier = 1.15
      }
    ]


{-| Calculate the current cost of starting the next craft project.
    Cost scales exponentially: baseCost * costMultiplier ^ owned
-}
currentCost : Project -> Int -> Float
currentCost project owned =
    project.baseCost * (project.costMultiplier ^ toFloat owned)


{-| Calculate total stitch output per second for a given count of projects.
-}
outputPerSecond : Project -> Int -> Float
outputPerSecond project count =
    project.baseOutput * toFloat count


{-| Find a project by its string ID.
-}
findById : String -> Maybe Project
findById id =
    allProjects
        |> List.filter (\p -> p.id == id)
        |> List.head
