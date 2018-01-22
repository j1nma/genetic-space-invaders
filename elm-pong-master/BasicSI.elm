module Main exposing (..)

import Element exposing (..)
import Char
import Time exposing (..)
import Window
import Html exposing (..)
import Keyboard exposing (..)
import Set exposing (Set)
import Task
import AnimationFrame
import Random exposing (..)
import Tuple
import Initial exposing (..)
import Constants exposing (..)
import Model exposing (..)
import Update exposing (..)
import View exposing (..)
import Collage exposing (..)


--import State exposing (state, andThen, State)


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


getInput : Game -> Float -> Input
getInput game delta =
    { space = Set.member (Char.toCode ' ') (game.keysDown)
    , reset = Set.member (Char.toCode 'R') (game.keysDown)
    , pause = Set.member (Char.toCode 'P') (game.keysDown)
    , start = Set.member (Char.toCode 'S') (game.keysDown)
    , dir =
        if Set.member 39 (game.keysDown) then
            1
            -- right arrow
        else if Set.member 37 (game.keysDown) then
            -1
            -- left arrow
        else
            0
    , delta = inSeconds delta
    }


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
                ( updateGame input game, Cmd.none )

        WindowResize dim ->
            ( { game | windowDimensions = dim }, Cmd.none )

        NoOp ->
            ( game, Cmd.none )


subscriptions _ =
    Sub.batch
        [ Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , Window.resizes sizeToMsg
        , AnimationFrame.diffs Tick
        ]


initialSizeCmd : Cmd Msg
initialSizeCmd =
    Task.perform sizeToMsg (Window.size)


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
updateGame { space, reset, pause, start, dir, delta } ({ state, spaceship, invaders, bullets } as game) =
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
                , invaders = initialInvader
                , bullets = initialBullet
            }
        else
            case state of
                Play ->
                    let
                        originalInvaders =
                            invaders

                        newBullet =
                            if space then
                                let
                                    _ =
                                        (Debug.log "craft hola" 1)
                                in
                                    craftBullet spaceship bullets
                            else
                                []
                    in
                        { game
                            | state = newState
                            , spaceship = updateSpaceship delta dir spaceship
                            , bullets = newBullet ++ updateBullets delta bullets originalInvaders
                            , invaders = updateInvaders delta invaders bullets
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
                            |> filled pongGreen
                         , rect 10 40
                            |> make spaceship
                         , toForm (statusMessage state)
                            |> move ( 0, 40 - gameHeight / 2 )
                         ]
                            ++ (List.map (\o -> make o (oval 10 40)) bullets)
                            ++ (List.map (\o -> make o (rect 40 40)) invaders)
                        )
        ]
