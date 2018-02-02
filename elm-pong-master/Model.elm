module Model exposing (..)

import Random exposing (..)
import Set exposing (Set)
import Keyboard exposing (..)
import Genetic exposing (..)
import GeneticHelper exposing (..)
import Time exposing (..)


type Msg
    = KeyDown KeyCode
    | KeyUp KeyCode
    | WindowResize ( Int, Int )
    | Tick Float
    | NoOp
    | OnTime Time


type alias Game =
    { keysDown : Set KeyCode
    , windowDimensions : ( Int, Int )
    , state : State
    , spaceship : Spaceship
    , invaders : List Invader
    , bullets : List Bullet
    , bestSolution : ( IntermediateValue Dna, Seed )
    , currentTime : Time
    , hasSpawned : Bool
    , score : Int
    }


type State
    = Play
    | Pause
    | Start
    | Over


type alias Spaceship =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    }


type alias Bullet =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , hit : Bool
    }


type alias Invader =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , xProbChange : Float
    , yProbChange : Float
    , seedX : Seed
    , seedY : Seed
    , wasHit : Bool
    }
