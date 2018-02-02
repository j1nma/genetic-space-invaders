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


filterInvaderHit : Invader -> Bool
filterInvaderHit invader =
    not invader.wasHit


filterBulletHit : Bullet -> Bool
filterBulletHit bullet =
    not bullet.hit


near : Float -> Float -> Float -> Bool
near k c n =
    -(k - n) <= c && c >= k - n


within : Bullet -> Invader -> Bool
within bullet invader =
    near invader.x 20 bullet.x && (invader.y - bullet.y) <= 40


withinBullet : Bullet -> Bullet -> Bool
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
