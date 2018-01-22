module Initial exposing (..)

import Random exposing (..)
import Set exposing (Set)
import Constants exposing (..)
import Model exposing (..)


initialSpaceship =
    { x = 0
    , y = (-halfHeight)
    , vx = 0
    , vy = 0
    }


initialInvader =
    [ { x = 0
      , y = 0
      , vx = -100
      , vy = 100
      , scale = 1
      , xProbChange = 0.01
      , yProbChange = 0.01
      , seedX = initialSeed 42
      , seedY = initialSeed 43
      }
    , { x = 10
      , y = 10
      , vx = 100
      , vy = -100
      , scale = 1
      , xProbChange = 0.01
      , yProbChange = 0.01
      , seedX = initialSeed 50
      , seedY = initialSeed 49
      }
    ]


initialBullet =
    []


initialGame =
    { keysDown = Set.empty
    , windowDimensions = ( 0, 0 )
    , state = Pause
    , spaceship = initialSpaceship
    , invaders = initialInvader
    , bullets = initialBullet
    }
