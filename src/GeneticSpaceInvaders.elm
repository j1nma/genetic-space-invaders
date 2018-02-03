module GeneticSpaceInvaders exposing (..)

import Element exposing (..)
import Char
import Time exposing (..)
import Window
import Html exposing (..)
import Keyboard exposing (..)
import Set exposing (Set)
import AnimationFrame
import Initial exposing (..)
import Constants exposing (..)
import Model exposing (..)
import Update exposing (..)
import View exposing (..)
import Collage exposing (..)
import GeneticHelper exposing (..)
import Genetic exposing (..)
import Random exposing (..)


main : Program Never Game Msg
main =
    program
        { init = ( initialGame, initialSizeCmd )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


getInput : Game -> Float -> Input
getInput game delta =
    { space = Set.member (Char.toCode ' ') (game.keysDown)
    , reset = Set.member (Char.toCode 'R') (game.keysDown)
    , pause = Set.member (Char.toCode 'P') (game.keysDown)
    , start = Set.member (Char.toCode 'S') (game.keysDown)
    , dir =
        if Set.member rightArrow (game.keysDown) then
            1
        else if Set.member leftArrow (game.keysDown) then
            -1
        else
            0
    , delta = inSeconds delta
    }


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        KeyDown key ->
            ( { game | keysDown = Set.insert key game.keysDown }, Cmd.none )

        KeyUp key ->
            ( { game | keysDown = Set.remove key game.keysDown }, Cmd.none )

        Tick delta ->
            let
                input =
                    getInput game (delta * 2)
            in
                ( updateGame input game, getTime )

        WindowResize dim ->
            ( { game | windowDimensions = dim }, Cmd.none )

        NoOp ->
            ( game, Cmd.none )

        OnTime t ->
            ( { game
                | currentTime = t
              }
            , Cmd.none
            )


subscriptions : a -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , Window.resizes sizeToMsg
        , AnimationFrame.diffs Tick
        ]



-- MODEL


type alias Input =
    { space : Bool
    , reset : Bool
    , pause : Bool
    , start : Bool
    , dir : Int
    , delta : Time
    }



-- UPDATE


updateGame : Input -> Game -> Game
updateGame { space, reset, pause, start, dir, delta } ({ state, spaceship, invaders, bullets, bestSolution, currentTime, hasSpawned, score } as game) =
    let
        newState =
            if start then
                Play
            else if pause then
                Pause
            else
                state
    in
        if reset then
            { game
                | state = Start
                , spaceship = initialSpaceship
                , invaders = []
                , bullets = initialBullet
                , bestSolution = initialEvolve (initialSeed (round currentTime))
                , hasSpawned = False
            }
        else
            case state of
                Play ->
                    let
                        newBullet =
                            if space then
                                craftBullet spaceship bullets
                            else
                                []

                        updatedInvaders =
                            updateInvaders delta invaders bullets

                        gameOver =
                            (List.length updatedInvaders == gameOverInvaders)
                    in
                        if (((round (inSeconds currentTime)) % 2) == 0) then
                            let
                                newFitness =
                                    calculateFitness (dnaFromValue (Tuple.first bestSolution)) updatedInvaders

                                updatedSolutionForFitness =
                                    updateSolution newFitness (Tuple.first (bestSolution))

                                betterSolution =
                                    if not hasSpawned then
                                        (GeneticHelper.evolve (Tuple.second (bestSolution)) (Tuple.first (bestSolution)))
                                    else
                                        bestSolution

                                betterDna =
                                    dnaFromValue (Tuple.first betterSolution)
                            in
                                { game
                                    | state =
                                        if gameOver then
                                            Over
                                        else
                                            newState
                                    , spaceship = updateSpaceship delta dir spaceship
                                    , bullets = newBullet ++ updateBullets delta bullets invaders
                                    , bestSolution = betterSolution
                                    , invaders =
                                        let
                                            _ =
                                                Debug.log "fitness" newFitness

                                            --_ =
                                            --    Debug.log "best" updatedSolutionForFitness
                                        in
                                            if not hasSpawned then
                                                updatedInvaders ++ spawnNewInvadersFromBestDna (Tuple.second (betterSolution)) newSpawnedInvaders betterDna
                                            else
                                                updatedInvaders
                                    , hasSpawned = True
                                    , score = score + ((List.length invaders) - (List.length updatedInvaders))
                                }
                        else
                            { game
                                | state = newState
                                , spaceship = updateSpaceship delta dir spaceship
                                , bullets = newBullet ++ updateBullets delta bullets invaders
                                , invaders = updatedInvaders
                                , hasSpawned = False
                                , score = score + ((List.length invaders) - (List.length updatedInvaders))
                            }

                Pause ->
                    { game | state = newState }

                Start ->
                    { game
                        | state =
                            case newState of
                                Pause ->
                                    Start

                                otherwise ->
                                    newState
                        , bestSolution = initialEvolve (initialSeed (round currentTime))
                    }

                Over ->
                    { game
                        | state =
                            case newState of
                                Play ->
                                    Over

                                Pause ->
                                    Over

                                Start ->
                                    Start

                                otherwise ->
                                    newState
                    }



-- VIEW


view : Game -> Html Msg
view { windowDimensions, state, spaceship, invaders, bullets, score } =
    div []
        [ let
            ( w, h ) =
                windowDimensions
          in
            toHtml <|
                container w h middle <|
                    collage gameWidth
                        gameHeight
                        ([ rect gameWidth gameHeight
                            |> filled blackBackground
                         , toForm (invadersStatus state (List.length invaders))
                            |> move ( -halfWidth + 140, halfHeight - 20 )
                         , toForm (scoreStatus state score)
                            |> move ( -halfWidth + 50, halfHeight - 20 )
                         ]
                            ++ (List.map (\o -> makeBullet o) bullets)
                            ++ (List.map (\o -> makeInvader o) invaders)
                            ++ [ makeSpaceship spaceship
                               , toForm (titleStatus state)
                                    |> move ( 0, 30 )
                               , toForm (messageStatus state)
                                    |> move ( 0, 80 - gameHeight / 2 )
                               ]
                        )
        ]
