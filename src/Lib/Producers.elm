module Lib.Producers exposing
    ( Producer
    , allProducers
    , currentCost
    , findById
    , outputPerSecond
    )

{-| Producer definitions for the Fiber Empire game.
    Producers generate Fiber passively over time.
-}


type alias Producer =
    { id : String
    , name : String
    , description : String
    , image : String
    , baseCost : Float
    , baseOutput : Float
    , costMultiplier : Float
    }


allProducers : List Producer
allProducers =
    [ { id = "sheep"
      , name = "Sheep"
      , description = "A fluffy friend. Produces a small amount of fiber."
      , image = "/images/sheep.png"
      , baseCost = 15
      , baseOutput = 0.1
      , costMultiplier = 1.15
      }
    , { id = "alpaca"
      , name = "Alpaca"
      , description = "Softer, fancier fleece. Produces more fiber."
      , image = "/images/alpaca.png"
      , baseCost = 100
      , baseOutput = 0.5
      , costMultiplier = 1.15
      }
    , { id = "spinning-wheel"
      , name = "Spinning Wheel"
      , description = "Turns raw fleece into usable fiber faster."
      , image = "/images/spinning-wheel.png"
      , baseCost = 500
      , baseOutput = 2
      , costMultiplier = 1.15
      }
    , { id = "yarn-shop"
      , name = "Yarn Shop"
      , description = "A cozy storefront that generates fiber steadily."
      , image = "/images/yarn-shop.png"
      , baseCost = 3000
      , baseOutput = 10
      , costMultiplier = 1.15
      }
    , { id = "fiber-farm"
      , name = "Fiber Farm"
      , description = "A whole pasture of fiber animals."
      , image = "/images/fiber-farm.png"
      , baseCost = 20000
      , baseOutput = 50
      , costMultiplier = 1.15
      }
    , { id = "fiber-mill"
      , name = "Fiber Mill"
      , description = "Industrial-scale fiber processing."
      , image = "/images/fiber-mill.png"
      , baseCost = 150000
      , baseOutput = 250
      , costMultiplier = 1.15
      }
    ]


{-| Calculate the current cost of purchasing the next unit of a producer.
    Cost scales exponentially: baseCost * costMultiplier ^ owned
-}
currentCost : Producer -> Int -> Float
currentCost producer owned =
    producer.baseCost * (producer.costMultiplier ^ toFloat owned)


{-| Calculate total output per second for a given count of producers.
-}
outputPerSecond : Producer -> Int -> Float
outputPerSecond producer count =
    producer.baseOutput * toFloat count


{-| Find a producer by its string ID.
-}
findById : String -> Maybe Producer
findById id =
    allProducers
        |> List.filter (\p -> p.id == id)
        |> List.head
