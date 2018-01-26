module Update exposing (..)

import Model exposing (..)
import Time exposing (..)
import Constants exposing (..)
import Random exposing (..)
import GeneticHelper exposing (Dna)
import Array exposing (..)


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
        { invader | x = outOfBounds, y = outOfBounds, fitness = 0.0 }
    else
        decideMovement t (increaseFitness invader)


increaseFitness : Invader -> Invader
increaseFitness invader =
    let
        oldFitness =
            invader.fitness
    in
        { invader | fitness = oldFitness + 1.0 }


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


physicsUpdate :
    number
    -> { a | vx : number, vy : number, x : number, y : number }
    -> { a | vx : number, vy : number, x : number, y : number }
physicsUpdate t ({ x, y, vx, vy } as obj) =
    { obj
        | x = x + vx * t
        , y = y + vy * t
    }


filterObject : { a | x : number1, y : number } -> Bool
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


spawnNewInvaders : Int -> List Dna -> List Invader
spawnNewInvaders time dnas =
    dnas
        |> List.map
            (\dna ->
                { x = 0.0
                , y = 0.0
                , vx = getValue (Array.get 2 (Array.fromList dna.dna))
                , vy = getValue (Array.get 3 (Array.fromList dna.dna))
                , xProbChange = getValue (Array.get 0 (Array.fromList dna.dna))
                , yProbChange = getValue (Array.get 1 (Array.fromList dna.dna))
                , seedX = initialSeed time
                , seedY = initialSeed (-time)
                , fitness = dna.fitness
                }
            )


spawnNewInvadersFromBestDna : Int -> Int -> Dna -> List Invader
spawnNewInvadersFromBestDna time amount dna =
    case amount of
        0 ->
            []

        n ->
            { x = 0.0
            , y = 0.0
            , vx = getValue (Array.get 2 (Array.fromList dna.dna))
            , vy = getValue (Array.get 3 (Array.fromList dna.dna))
            , xProbChange = getValue (Array.get 0 (Array.fromList dna.dna))
            , yProbChange = getValue (Array.get 1 (Array.fromList dna.dna))
            , seedX = initialSeed time
            , seedY = initialSeed (-time)
            , fitness = dna.fitness
            }
                :: spawnNewInvadersFromBestDna (time + 1) (n - 1) dna


getValue : Maybe Float -> Float
getValue m =
    case m of
        Just v ->
            v

        Nothing ->
            Debug.crash "No value on dna list!"
