module Update exposing (..)

import Model exposing (..)
import Time exposing (..)
import Constants exposing (..)
import Random exposing (..)
import GeneticHelper exposing (Dna)
import Array exposing (..)
import Auxiliary exposing (..)


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
        aux =
            List.filter filterObject (List.map (\i -> updateInvader t bullets i) invaders)
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


updateBullets : Time -> List Bullet -> List Invader -> List Bullet
updateBullets t bullets invaders =
    let
        aux =
            List.filter filterObject (List.map (\b -> updateBullet t invaders b) bullets)
    in
        aux


updateBullet : Time -> List Invader -> Bullet -> Bullet
updateBullet t invaders bullet =
    if (not (bullet.y |> near 0 halfHeight)) || (not (List.isEmpty (List.filter (\i -> within bullet i) invaders))) then
        { bullet | x = outOfBounds, y = outOfBounds }
    else
        physicsUpdate t bullet


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
