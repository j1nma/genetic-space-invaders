module Initial exposing (..)

import Set exposing (Set)
import Constants exposing (..)
import Model exposing (..)
import GeneticHelper exposing (..)
import Update exposing (..)
import Genetic exposing (..)
import Time exposing (..)
import Random exposing (..)
import Task
import Window


initialGame : Game
initialGame =
    { keysDown = Set.empty
    , windowDimensions = ( 0, 0 )
    , state = Start
    , spaceship = initialSpaceship
    , invaders = []
    , bullets = initialBullet
    , bestSolution = initialEvolve (initialSeed 0)
    , currentTime = 0.0
    , hasSpawned = False
    , score = 0
    }


initialSpaceship : { vx : Float, vy : Float, x : Float, y : Float }
initialSpaceship =
    { x = 0
    , y = (-halfHeight)
    , vx = 0
    , vy = 0
    }


initialBullet : List a
initialBullet =
    []


initialInvaders : Seed -> List Invader
initialInvaders seed =
    spawnNewInvadersFromBestDna seed newSpawnedInvaders (initialDna seed)


initialSizeCmd : Cmd Msg
initialSizeCmd =
    Task.perform sizeToMsg Window.size


getTime : Cmd Msg
getTime =
    Task.perform OnTime Time.now


sizeToMsg : Window.Size -> Msg
sizeToMsg size =
    WindowResize ( size.width, size.height )
