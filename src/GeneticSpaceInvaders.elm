module GeneticSpaceInvaders exposing (..)

import Element exposing (..)
import Char
import Time exposing (..)
import Window
import Html exposing (..)
import Keyboard exposing (..)
import Set
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
    { space = Set.member (Char.toCode ' ') game.keysDown
    , reset = Set.member (Char.toCode 'R') game.keysDown
    , pause = Set.member (Char.toCode 'P') game.keysDown
    , start = Set.member (Char.toCode 'S') game.keysDown
    , dir =
        if Set.member rightArrow game.keysDown then
            1
        else if Set.member leftArrow game.keysDown then
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


updateGame : Input -> Game -> Game
updateGame ({ reset, pause, start } as input) ({ windowDimensions, state, currentTime } as game) =
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
            resetGame currentTime windowDimensions
        else
            updateState newState input game


updateState : State -> Input -> Game -> Game
updateState newState { space, dir, delta } ({ state, spaceship, invaders, bullets, bestSolution, currentTime, hasSpawned, score } as game) =
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
                    List.length updatedInvaders >= gameOverInvaders

                newScore =
                    List.length invaders - List.length updatedInvaders

                newFitness =
                    calculateFitness (dnaFromValue (Tuple.first bestSolution)) updatedInvaders

                updatedSolutionForFitness =
                    updateSolution newFitness bestSolution
            in
                if (round (inSeconds currentTime) % 2) == 0 then
                    let
                        betterSolution =
                            if not hasSpawned then
                                GeneticHelper.evolve updatedSolutionForFitness
                            else
                                updatedSolutionForFitness

                        newInvaders =
                            if not hasSpawned then
                                updatedInvaders ++ spawnNewInvadersFromBestDna betterSolution newSpawnedInvaders
                            else
                                updatedInvaders
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
                            , invaders = newInvaders
                            , hasSpawned = True
                            , score = score + newScore
                        }
                else
                    { game
                        | state =
                            if gameOver then
                                Over
                            else
                                newState
                        , spaceship = updateSpaceship delta dir spaceship
                        , bullets = newBullet ++ updateBullets delta bullets invaders
                        , bestSolution = updatedSolutionForFitness
                        , invaders = updatedInvaders
                        , hasSpawned = False
                        , score = score + newScore
                    }

        Pause ->
            { game | state = newState }

        Start ->
            { game
                | state =
                    case newState of
                        Pause ->
                            Start

                        _ ->
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

                        _ ->
                            newState
            }


view : Game -> Html Msg
view { windowDimensions, state, spaceship, invaders, bullets, score, currentTime } =
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
                            ++ List.map (\o -> makeBullet o) bullets
                            ++ List.map (\o -> makeInvader o) invaders
                            ++ [ makeSpaceship spaceship
                               , toForm (titleStatus state currentTime)
                                    |> move ( 0, 0 )
                               , toForm (shuttleTitleStatus state)
                                    |> move ( 0, halfHeight - 90 )
                               , toForm (messageStatus state)
                                    |> move ( 0, 80 - gameHeight / 2 )
                               ]
                        )
        ]
