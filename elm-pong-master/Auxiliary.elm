module Auxiliary exposing (..)

import Model exposing (..)
import Time exposing (..)
import Constants exposing (..)
import Random exposing (..)


randomMovement : Time -> Invader -> Invader
randomMovement t invader =
    let
        changeX =
            probDirChange invader.seedX invader.xProbChange

        newVelX =
            invader.vx * Tuple.first changeX

        changeY =
            probDirChange invader.seedY invader.yProbChange

        newVelY =
            invader.vy * Tuple.first changeY
    in
        physicsUpdate t { invader | vx = newVelX, vy = newVelY, seedX = (Tuple.second changeX), seedY = (Tuple.second changeY) }


stepV : number -> Bool -> Bool -> number
stepV v lowerCollision upperCollision =
    if lowerCollision then
        abs v
    else if upperCollision then
        0 - abs v
    else
        v


decideMovement : Time -> Invader -> Invader
decideMovement t invader =
    let
        leftCollision =
            near invader.x 2 (-halfWidth + 20)

        rightCollision =
            near invader.x 2 (halfWidth - 20)

        upperCollision =
            near invader.y 2 (halfHeight - 20)

        lowerCollision =
            near invader.y 2 (-halfHeight + 60)
    in
        if leftCollision || rightCollision || upperCollision || lowerCollision then
            physicsUpdate t { invader | vx = stepV invader.vx leftCollision rightCollision, vy = stepV invader.vy lowerCollision upperCollision }
        else
            randomMovement t invader


probDirChange : Seed -> Float -> ( Float, Seed )
probDirChange seed p =
    let
        generator =
            float 0 1

        ( addProbability, s ) =
            step generator seed
    in
        if p > addProbability then
            ( (-1), s )
        else
            ( 1, s )


physicsUpdate :
    Float
    -> { a | vx : Float, vy : Float, x : Float, y : Float }
    -> { a | vx : Float, vy : Float, x : Float, y : Float }
physicsUpdate t ({ x, y, vx, vy } as obj) =
    { obj
        | x = x + vx * t
        , y = y + vy * t
    }


filterObject : { a | x : Float, y : Float } -> Bool
filterObject ({ x, y } as obj) =
    if obj.x == outOfBounds && obj.y == outOfBounds then
        False
    else
        True


near : Float -> Float -> Float -> Bool
near k c n =
    n >= k - c && n <= k + c


within : { a | x : Float, y : Float } -> { b | x : Float, y : Float } -> Bool
within bullet invader =
    near invader.x 20 bullet.x && near invader.y 20 bullet.y


withinBullet :
    { a | x : Float, y : Float }
    -> { b | x : Float, y : Float }
    -> Bool
withinBullet b1 b2 =
    near b1.x 30 b2.x && near b1.y 30 b2.y


getValue : Maybe Float -> Float
getValue m =
    case m of
        Just v ->
            v

        Nothing ->
            Debug.crash "No value on dna list!"


randomPosition : Seed -> ( ( Float, Float ), Seed )
randomPosition seed =
    let
        randomWidthGenerator =
            Random.float (-halfWidth + 20) (halfWidth - 20)

        randomHeightGenerator =
            Random.float (-halfHeight + 60) (halfHeight - 20)
    in
        let
            ( w, seedY ) =
                Random.step randomWidthGenerator seed

            ( h, newSeed ) =
                Random.step randomHeightGenerator seedY
        in
            ( ( w, h ), newSeed )
