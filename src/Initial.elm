module Initial exposing (..)

import Set
import Constants exposing (..)
import Model exposing (..)
import GeneticHelper exposing (..)
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
    , bullets = []
    , bestSolution = initialEvolve (initialSeed 0)
    , currentTime = 0.0
    , hasSpawned = False
    , score = 0
    }


resetGame : Time -> ( Int, Int ) -> Game
resetGame currentTime windowDimensions =
    { keysDown = Set.empty
    , windowDimensions = windowDimensions
    , state = Start
    , spaceship = initialSpaceship
    , invaders = []
    , bullets = []
    , bestSolution = initialEvolve (initialSeed (round currentTime))
    , hasSpawned = False
    , currentTime = currentTime
    , score = 0
    }


initialSpaceship : { vx : Float, vy : Float, x : Float, y : Float }
initialSpaceship =
    { x = 0
    , y = -halfHeight
    , vx = 0
    , vy = 0
    }


initialSizeCmd : Cmd Msg
initialSizeCmd =
    Task.perform sizeToMsg Window.size


getTime : Cmd Msg
getTime =
    Task.perform OnTime Time.now


sizeToMsg : Window.Size -> Msg
sizeToMsg size =
    WindowResize ( size.width, size.height )
