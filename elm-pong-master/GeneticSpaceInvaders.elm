module GeneticSpaceInvaders exposing (..)

import Element exposing (..)
import Char
import Time exposing (..)
import Window
import Html exposing (..)
import Keyboard exposing (..)
import Set exposing (Set)
import Task
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


main :
    Program Never
        { bestSolution : ( IntermediateValue Dna, Seed )
        , bullets : List Bullet
        , currentTime : Time
        , invaders : List Invader
        , keysDown : Set KeyCode
        , spaceship : { vx : Float, vy : Float, x : Float, y : Float }
        , state : State
        , windowDimensions : ( Int, Int )
        , hasSpawned : Bool
        }
        Msg
main =
    program
        { init = ( initialGame, initialSizeCmd )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = KeyDown KeyCode
    | KeyUp KeyCode
    | WindowResize ( Int, Int )
    | Tick Float
    | NoOp
    | OnTime Time


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


update :
    Msg
    ->
        { bestSolution : ( IntermediateValue Dna, Seed )
        , bullets : List Bullet
        , currentTime : Time
        , invaders : List Invader
        , keysDown : Set KeyCode
        , spaceship : { vx : Float, vy : Float, x : Float, y : Float }
        , state : State
        , windowDimensions : ( Int, Int )
        , hasSpawned : Bool
        }
    ->
        ( { bestSolution : ( IntermediateValue Dna, Seed )
          , bullets : List Bullet
          , currentTime : Time
          , invaders : List Invader
          , keysDown : Set KeyCode
          , spaceship : { vx : Float, vy : Float, x : Float, y : Float }
          , state : State
          , windowDimensions : ( Int, Int )
          , hasSpawned : Bool
          }
        , Cmd Msg
        )
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


initialSizeCmd : Cmd Msg
initialSizeCmd =
    Task.perform sizeToMsg Window.size


getTime : Cmd Msg
getTime =
    Task.perform OnTime Time.now



--initialCmd : Cmd Msg
--initialCmd =
--    Task.perform InitialSetup (Task.map2 (\size time -> { initialDim = size, initialTime = time }) (Window.size) (Time.now))


sizeToMsg : Window.Size -> Msg
sizeToMsg size =
    WindowResize ( size.width, size.height )



-- MODEL


type alias Game =
    { keysDown : Set KeyCode
    , windowDimensions : ( Int, Int )
    , state : State
    , spaceship : Spaceship
    , invaders : List Invader
    , bullets : List Bullet
    , bestSolution : ( IntermediateValue Dna, Seed )
    , currentTime : Time
    , hasSpawned : Bool
    }


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
updateGame { space, reset, pause, start, dir, delta } ({ state, spaceship, invaders, bullets, bestSolution, currentTime, hasSpawned } as game) =
    let
        newState =
            if start then
                Play
            else if (pause) then
                Pause
            else
                state
    in
        if reset then
            { game
                | state = Pause
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
                        currentSolution =
                            if List.length invaders == 0 then
                                initialEvolve (initialSeed (round currentTime))
                            else
                                bestSolution

                        originalInvaders =
                            invaders

                        newBullet =
                            if space then
                                craftBullet spaceship bullets
                            else
                                []

                        updatedInvaders =
                            updateInvaders delta invaders bullets
                    in
                        if (((round (inSeconds currentTime)) % 2) == 0) then
                            let
                                newFitness =
                                    calculateFitness (dnaFromValue (Tuple.first currentSolution)) updatedInvaders

                                updatedSolutionForFitness =
                                    updateSolution newFitness (Tuple.first (currentSolution))

                                betterSolution =
                                    if not hasSpawned then
                                        (GeneticHelper.evolve (Tuple.second (currentSolution)) (Tuple.first (currentSolution)))
                                    else
                                        currentSolution

                                betterDna =
                                    dnaFromValue (Tuple.first betterSolution)
                            in
                                { game
                                    | state = newState
                                    , spaceship = updateSpaceship delta dir spaceship
                                    , bullets = newBullet ++ updateBullets delta bullets originalInvaders
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
                                }
                        else
                            { game
                                | state = newState
                                , spaceship = updateSpaceship delta dir spaceship
                                , bullets = newBullet ++ updateBullets delta bullets originalInvaders
                                , invaders = updatedInvaders
                                , hasSpawned = False
                            }

                Pause ->
                    { game | state = newState }



-- VIEW


view : Game -> Html Msg
view { windowDimensions, state, spaceship, invaders, bullets } =
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
                         ]
                            ++ (List.map (\o -> makeBullet o) bullets)
                            ++ (List.map (\o -> makeInvader o) invaders)
                            ++ [ makeSpaceship spaceship
                               , toForm (statusTitle state)
                                    |> move ( 0, 30 )
                               , toForm (statusMessage state)
                                    |> move ( 0, 80 - gameHeight / 2 )
                               ]
                        )
        ]
