module Initial exposing (..)

import Set exposing (Set)
import Constants exposing (..)
import Model exposing (..)
import GeneticHelper exposing (..)
import Update exposing (..)
import Genetic exposing (..)
import Time exposing (..)


initialSpaceship : { vx : Float, vy : Float, x : Float, y : Float }
initialSpaceship =
    { x = 0
    , y = (-halfHeight)
    , vx = 0
    , vy = 0
    }


initialInvaders : Int -> List Invader
initialInvaders time =
    spawnNewInvadersFromBestDna time 10 (initialDna time)


initialBullet : List a
initialBullet =
    []


initialGame :
    { bullets : List b
    , invaders : List Invader
    , keysDown : Set a
    , spaceship : { vx : Float, vy : Float, x : Float, y : Float }
    , state : State
    , windowDimensions : ( Int, Int )
    , bestSolution : Genetic.IntermediateValue Dna
    , currentTime : Time
    , hasSpawned : Bool
    }
initialGame =
    { keysDown = Set.empty
    , windowDimensions = ( 0, 0 )
    , state = Pause
    , spaceship = initialSpaceship
    , invaders = initialInvaders 1212
    , bullets = initialBullet
    , bestSolution = initialEvolve 1212
    , currentTime = 0.0
    , hasSpawned = False
    }
