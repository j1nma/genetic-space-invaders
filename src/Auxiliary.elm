module Auxiliary exposing (..)

import Model exposing (..)
import Time exposing (..)
import Constants exposing (..)
import Random exposing (..)


probDirChange : Seed -> Float -> ( Float, Seed )
probDirChange seed p =
    let
        generator =
            float 0.25 1

        ( addProbability, s ) =
            step generator seed
    in
        if p > addProbability then
            ( -1, s )
        else
            ( 1, s )


physicsUpdate : Float -> { a | vx : Float, vy : Float, x : Float, y : Float } -> { a | vx : Float, vy : Float, x : Float, y : Float }
physicsUpdate t ({ x, y, vx, vy } as obj) =
    { obj
        | x = x + (vx * t)
        , y = y + (vy * t)
    }


near : Float -> Float -> Float -> Bool
near k c n =
    abs (k - n) <= c


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
        physicsUpdate t { invader | vx = newVelX, vy = newVelY, seedX = Tuple.second changeX, seedY = Tuple.second changeY }


stepV : Float -> Bool -> Bool -> Float
stepV v negativeCollision positiveCollision =
    if negativeCollision then
        abs v
    else if positiveCollision then
        0 - abs v
    else
        v


decideMovement : Time -> Invader -> Invader
decideMovement t invader =
    let
        leftCollision =
            near invader.x 10 -halfWidth

        rightCollision =
            near invader.x 10 halfWidth

        upperCollision =
            near invader.y 10 halfHeight

        lowerCollision =
            near invader.y 60 -halfHeight
    in
        if leftCollision || rightCollision || upperCollision || lowerCollision then
            physicsUpdate t { invader | vx = stepV invader.vx leftCollision rightCollision, vy = stepV invader.vy lowerCollision upperCollision }
        else
            randomMovement t invader


within : Bullet -> Invader -> Bool
within bullet invader =
    let
        diff =
            invader.y - bullet.y
    in
        near invader.x 20 bullet.x && 0 <= diff && diff <= 40


withinBullet : Bullet -> Bullet -> Bool
withinBullet b1 b2 =
    near b1.x 70 b2.x && near b1.y 70 b2.y


randomXCoordinate : Seed -> ( Float, Seed )
randomXCoordinate seed =
    let
        randomXGenerator =
            Random.float (-halfWidth + 20) (halfWidth - 20)
    in
        Random.step randomXGenerator seed


randomYCoordinate : Seed -> ( Float, Seed )
randomYCoordinate seed =
    let
        randomYGenerator =
            Random.float (-halfHeight + 60) (halfHeight - 20)
    in
        Random.step randomYGenerator seed
