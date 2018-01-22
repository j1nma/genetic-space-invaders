module Update exposing (..)

import Model exposing (..)
import Time exposing (..)
import Constants exposing (..)
import Random exposing (..)


craftBullet : Spaceship -> List Bullet -> List Bullet
craftBullet spaceship bullets =
    let
        newBullet =
            { x = spaceship.x
            , y = spaceship.y
            , vx = 0
            , vy = 200
            }
    in
        if List.any (checkBullet newBullet) bullets then
            []
        else
            [ newBullet ]


checkBullet : Bullet -> Bullet -> Bool
checkBullet b1 b2 =
    withinBullet b1 b2


updateSpaceship : Time -> Int -> Spaceship -> Spaceship
updateSpaceship t dir spaceship =
    let
        spaceship1 =
            physicsUpdate t { spaceship | vx = toFloat dir * 200 }
    in
        { spaceship1
            | x = clamp (22 - halfWidth) (halfWidth - 22) spaceship1.x
        }


updateInvaders : Time -> List Invader -> List Bullet -> List Invader
updateInvaders t invaders bullets =
    let
        _ =
            Debug.log "invaders before:" (List.length invaders)
    in
        let
            aux =
                List.filter filterObject (List.map (\i -> updateInvader t bullets i) invaders)
        in
            let
                _ =
                    Debug.log "invaders after:" (List.length aux)
            in
                aux


updateInvader : Time -> List Bullet -> Invader -> Invader
updateInvader t bullets invader =
    if (not (List.isEmpty (List.filter (\b -> within invader b) bullets))) then
        { invader | x = outOfBounds, y = outOfBounds }
    else
        decideMovement t invader


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
            near invader.x 2 (-halfWidth)

        rightCollision =
            near invader.x 2 halfWidth

        upperCollision =
            near invader.y 2 halfHeight

        lowerCollision =
            near invader.y 2 (-halfHeight)
    in
        if leftCollision || rightCollision || upperCollision || lowerCollision then
            let
                _ =
                    Debug.log "vx before:" invader.vx

                _ =
                    Debug.log "vy before:" invader.vy
            in
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


updateBullets : Time -> List Bullet -> List Invader -> List Bullet
updateBullets t bullets invaders =
    let
        _ =
            List.length (Debug.log "bullets before:" bullets)
    in
        let
            aux =
                List.filter filterObject (List.map (\b -> updateBullet t invaders b) bullets)
        in
            let
                _ =
                    List.length (Debug.log "bullets after:" aux)
            in
                aux


updateBullet : Time -> List Invader -> Bullet -> Bullet
updateBullet t invaders bullet =
    if (not (bullet.y |> near 0 halfHeight)) || (not (List.isEmpty (List.filter (\i -> within bullet i) invaders))) then
        { bullet | x = outOfBounds, y = outOfBounds }
    else
        physicsUpdate t bullet


physicsUpdate t ({ x, y, vx, vy } as obj) =
    { obj
        | x = x + vx * t
        , y = y + vy * t
    }


filterObject ({ x, y } as obj) =
    if obj.x == outOfBounds && obj.y == outOfBounds then
        False
    else
        True


near : Float -> Float -> Float -> Bool
near k c n =
    n >= k - c && n <= k + c


within bullet invader =
    near invader.x 20 bullet.x && near invader.y 20 bullet.y


withinBullet b1 b2 =
    near b1.x 30 b2.x && near b1.y 30 b2.y
