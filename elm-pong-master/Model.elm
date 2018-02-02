module Model exposing (..)

import Random exposing (..)


type State
    = Play
    | Pause
    | Start


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
    }
