-- See this document for more information on making Pong:
-- http://elm-lang.org/blog/Pong.elm


module Main exposing (..)

import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Text
import Char
import Time exposing (..)
import Window
import Html exposing (..)
import Keyboard exposing (..)
import Set exposing (Set)
import Task
import AnimationFrame


main =
    program
        { init = ( initialGame, initialSizeCmd )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- KeyDown/KeyUp/keysDown technique taken from this answer :
--     http://stackoverflow.com/a/39127092/509928
--
-- to this question :
--     http://stackoverflow.com/questions/39125989/keyboard-combinations-in-elm-0-17-and-later
--


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
        if Set.member 38 (game.keysDown) then
            1
            -- down arrow
        else if Set.member 40 (game.keysDown) then
            -1
            -- up arrow
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
                    getInput game delta
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



-- initialSizeCmd/sizeToMsg technique taken from this answer :
--     https://www.reddit.com/r/elm/comments/4jfo32/getting_the_initial_window_dimensions/d369kw1/
--
-- to this question :
--     https://www.reddit.com/r/elm/comments/4jfo32/getting_the_initial_window_dimensions/


initialSizeCmd : Cmd Msg
initialSizeCmd =
    Task.perform sizeToMsg (Window.size)


sizeToMsg : Window.Size -> Msg
sizeToMsg size =
    WindowResize ( size.width, size.height )



-- MODEL


( gameWidth, gameHeight ) =
    ( 600, 400 )
( halfWidth, halfHeight ) =
    ( gameWidth / 2, gameHeight / 2 )


outOfBounds =
    1000


type State
    = Play
    | Pause


type alias Bullet =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    }


type alias Invader =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , lifetime : Int
    , size : Float
    }


type alias Spaceship =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , score : Int
    }


type alias Game =
    { keysDown : Set KeyCode
    , windowDimensions : ( Int, Int )
    , state : State
    , spaceship : Spaceship
    , invaders : List Invader
    , bullets : List Bullet
    }


initialSpaceship =
    { x = 0
    , y = (-halfHeight)
    , vx = 0
    , vy = 0
    , score = 0
    }


initialInvader =
    [ { x = 0
      , y = 0
      , vx = 0
      , vy = 0
      , lifetime = 0
      , size = 1
      }
    ]


initialBullet =
    [ { x = 0
      , y = 0
      , vx = 0
      , vy = 1
      }
    ]


initialGame =
    { keysDown = Set.empty
    , windowDimensions = ( 0, 0 )
    , state = Pause
    , spaceship = initialSpaceship
    , invaders = initialInvader
    , bullets = initialBullet
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
        score =
            0
    in
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
                let
                    newBullet =
                        if space then
                            craftBullet spaceship
                        else
                            []
                in
                    { game
                        | state = newState
                        , spaceship = updateSpaceship delta dir score spaceship
                        , invaders = updateInvaders delta invaders bullets
                        , bullets = newBullet ++ updateBullets delta bullets invaders
                    }


craftBullet : Spaceship -> List Bullet
craftBullet spaceship =
    [ { x = spaceship.x
      , y = spaceship.y
      , vx = 0
      , vy = 1
      }
    ]


updateSpaceship : Time -> Int -> Int -> Spaceship -> Spaceship
updateSpaceship t dir points spaceship =
    let
        spaceship1 =
            physicsUpdate t { spaceship | vx = toFloat dir * 200 }
    in
        { spaceship1
            | x = clamp (22 - halfWidth) (halfWidth - 22) spaceship1.x
            , score = spaceship.score + points
        }


updateInvaders : Time -> List Invader -> List Bullet -> List Invader
updateInvaders t invaders bullets =
    (List.map (\i -> updateInvader t bullets i) invaders)


updateInvader : Time -> List Bullet -> Invader -> Invader
updateInvader t bullets invader =
    if (not (List.isEmpty (List.filter (\b -> within invader b) bullets))) then
        { invader | x = outOfBounds, y = outOfBounds }
    else
        invader


updateBullets : Time -> List Bullet -> List Invader -> List Bullet
updateBullets t bullets invaders =
    List.filter filterBullet (List.map (\b -> updateBullet t invaders b) bullets)


filterBullet : Bullet -> Bool
filterBullet bullet =
    if bullet.x == outOfBounds && bullet.y == outOfBounds then
        False
    else
        True


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


near : Float -> Float -> Float -> Bool
near k c n =
    n >= k - c && n <= k + c


within bullet invader =
    near invader.x 8 bullet.x && near invader.y 20 bullet.y


stepV v lowerCollision upperCollision =
    if lowerCollision then
        abs v
    else if upperCollision then
        0 - abs v
    else
        v



-- VIEW


view : Game -> Html Msg
view { windowDimensions, state, spaceship, invaders, bullets } =
    let
        scores : Element
        scores =
            txt (Text.height 50) (toString spaceship.score)

        ( w, h ) =
            windowDimensions
    in
        toHtml <|
            container w h middle <|
                collage gameWidth
                    gameHeight
                    ([ rect gameWidth gameHeight
                        |> filled pongGreen
                     , verticalLine gameHeight
                        |> traced (dashed red)
                     , rect 10 40
                        |> make spaceship
                     , toForm scores
                        |> move ( 0, gameHeight / 2 - 40 )
                     , toForm (statusMessage state)
                        |> move ( 0, 40 - gameHeight / 2 )
                     ]
                        ++ (List.map (\o -> make o (rect 10 40)) invaders)
                    )


statusMessage state =
    case state of
        Play ->
            txt identity ""

        Pause ->
            txt identity pauseMessage


verticalLine height =
    path [ ( 0, height ), ( 0, -height ) ]


pongGreen =
    rgb 60 100 60


textGreen =
    rgb 160 200 160


txt f =
    Text.fromString >> Text.color textGreen >> Text.monospace >> f >> leftAligned


pauseMessage =
    "SPACE to start, P to pause, R to reset, WS and &uarr;&darr; to move"


make obj shape =
    shape
        |> filled white
        |> move ( obj.x, obj.y )
