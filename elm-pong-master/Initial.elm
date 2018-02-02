module Initial exposing (..)

import Set exposing (Set)
import Constants exposing (..)
import Model exposing (..)
import GeneticHelper exposing (..)
import Update exposing (..)
import Genetic exposing (..)
import Time exposing (..)
import Random exposing (..)


initialSpaceship : { vx : Float, vy : Float, x : Float, y : Float }
initialSpaceship =
    { x = 0
    , y = (-halfHeight)
    , vx = 0
    , vy = 0
    }


initialInvaders : Seed -> List Invader
initialInvaders seed =
    spawnNewInvadersFromBestDna seed newSpawnedInvaders (initialDna seed)


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
    , bestSolution : ( Genetic.IntermediateValue Dna, Seed )
    , currentTime : Time
    , hasSpawned : Bool
    }
initialGame =
    { keysDown = Set.empty
    , windowDimensions = ( 0, 0 )
    , state = Pause
    , spaceship = initialSpaceship
    , invaders = []
    , bullets = initialBullet
    , bestSolution = initialEvolve (initialSeed 0)
    , currentTime = 0.0
    , hasSpawned = False
    }
