module Constants exposing (..)

-- GAME


rightArrow : Int
rightArrow =
    39


leftArrow : Int
leftArrow =
    37



-- MODEL


( gameWidth, gameHeight ) =
    ( 600, 400 )
( halfWidth, halfHeight ) =
    ( gameWidth / 2, gameHeight / 2 )


outOfBounds : Float
outOfBounds =
    1000


newSpawnedInvaders : Int
newSpawnedInvaders =
    1
