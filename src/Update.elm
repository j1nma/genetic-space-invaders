module Update exposing (..)

import Model exposing (..)
import Time exposing (..)
import Constants exposing (..)
import Random exposing (..)
import GeneticHelper exposing (Dna)
import Array exposing (..)
import Auxiliary exposing (..)
import Genetic exposing (..)


craftBullet : Spaceship -> List Bullet -> List Bullet
craftBullet spaceship bullets =
    let
        newBullet =
            { x = spaceship.x
            , y = spaceship.y
            , vx = 0
            , vy = 200
            , hit = False
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
    List.filter (\i -> not i.wasHit) (List.map (\i -> updateInvader t bullets i) invaders)


updateInvader : Time -> List Bullet -> Invader -> Invader
updateInvader t bullets invader =
    if List.any (\b -> within b invader) bullets then
        { invader | wasHit = True }
    else
        decideMovement t invader


updateBullets : Time -> List Bullet -> List Invader -> List Bullet
updateBullets t bullets invaders =
    List.filter (\b -> not b.hit) (List.map (\b -> updateBullet t invaders b) bullets)


updateBullet : Time -> List Invader -> Bullet -> Bullet
updateBullet t invaders bullet =
    if (bullet.y >= (halfHeight - 40)) || (List.any (\i -> within bullet i) invaders) then
        { bullet | hit = True }
    else
        physicsUpdate t bullet


spawnNewInvadersFromBestDna : Seed -> Int -> Dna -> List Invader
spawnNewInvadersFromBestDna seed amount dna =
    case amount of
        0 ->
            []

        n ->
            let
                newX =
                    randomXCoordinate seed

                newXCoordinate =
                    Tuple.first newX

                newSeed =
                    Tuple.second newX

                newY =
                    randomYCoordinate newSeed

                newYCoordinate =
                    Tuple.first newY

                newestSeed =
                    Tuple.second newY
            in
                { x = newXCoordinate
                , y = newYCoordinate
                , vx = getValue (Array.get 2 (Array.fromList dna.genes))
                , vy = getValue (Array.get 3 (Array.fromList dna.genes))
                , xProbChange = getValue (Array.get 0 (Array.fromList dna.genes))
                , yProbChange = getValue (Array.get 1 (Array.fromList dna.genes))
                , seedX = newSeed
                , seedY = newestSeed
                , wasHit = False
                }
                    :: spawnNewInvadersFromBestDna newestSeed (n - 1) dna


calculateFitness : Dna -> List Invader -> Float
calculateFitness dna invaders =
    toFloat (List.length (List.filter (\invader -> invader.xProbChange == (getValue (Array.get 0 (Array.fromList dna.genes))) || invader.yProbChange == (getValue (Array.get 1 (Array.fromList dna.genes))) || invader.vx == (getValue (Array.get 2 (Array.fromList dna.genes))) || invader.vy == (getValue (Array.get 3 (Array.fromList dna.genes)))) invaders))


updateSolution : Float -> IntermediateValue Dna -> IntermediateValue Dna
updateSolution newFitness (IntermediateValue p pd ng) =
    (IntermediateValue p { dna = { genes = pd.dna.genes, fitness = newFitness }, points = newFitness } ng)
