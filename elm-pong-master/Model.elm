module Model exposing (..)

import Random exposing (..)


type State
    = Play
    | Pause


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
    , scale : Float
    , xProbChange : Float
    , yProbChange : Float
    , seedX : Seed
    , seedY : Seed
    }