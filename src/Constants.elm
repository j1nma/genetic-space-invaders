module Constants exposing (..)

-- GAME


rightArrow : Int
rightArrow =
    39


leftArrow : Int
leftArrow =
    37


gameOverInvaders : Int
gameOverInvaders =
    50


spawnFrenquencyInSeconds : Int
spawnFrenquencyInSeconds =
    2



-- MODEL


( gameWidth, gameHeight ) =
    ( 600, 400 )
( halfWidth, halfHeight ) =
    ( gameWidth / 2, gameHeight / 2 )


newSpawnedInvaders : Int
newSpawnedInvaders =
    4



-- UPDATE


spaceshipVelocity : Float
spaceshipVelocity =
    170.0
