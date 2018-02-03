module View exposing (..)

import Model exposing (..)
import Color exposing (..)
import Collage exposing (..)
import Text
import Element exposing (..)
import Time exposing (..)
import Constants exposing (..)


messageStatus : State -> Element
messageStatus state =
    case state of
        Play ->
            txt identity ""

        Pause ->
            txt (Text.italic >> Text.color yellow) pauseMessage

        Start ->
            txt (Text.italic >> Text.height 12 >> Text.color yellow) startMessage

        Over ->
            txt (Text.italic >> Text.bold >> Text.color red >> Text.height 18) overMessage


titleStatus : State -> Float -> Element
titleStatus state currentTime =
    case state of
        Start ->
            if (((round (inSeconds currentTime)) % 2) == 0) then
                image 600 400 "src/res/mainTitle.png"
            else
                image 600 400 "src/res/mainTitle2.png"

        otherwise ->
            txt identity ""


shuttleTitleStatus : State -> Float -> Element
shuttleTitleStatus state currentTime =
    case state of
        Start ->
            image 150 150 "src/res/lambdaSpaceshuttleTitle.png"

        otherwise ->
            txt identity ""


invadersStatus : State -> Int -> Element
invadersStatus state numberOfInvaders =
    case state of
        Start ->
            txt identity ""

        otherwise ->
            txt (Text.color yellow) ("INVADERS: " ++ (toString numberOfInvaders))


scoreStatus : State -> Int -> Element
scoreStatus state score =
    case state of
        Start ->
            txt identity ""

        otherwise ->
            txt (Text.color yellow) ("SCORE: " ++ (toString score))


verticalLine : Float -> Path
verticalLine height =
    path [ ( 0, height ), ( 0, -height ) ]


blackBackground : Color
blackBackground =
    rgb 0 0 0


txt : (Text.Text -> Text.Text) -> String -> Element
txt f =
    Text.fromString >> Text.monospace >> f >> leftAligned


startMessage : String
startMessage =
    "S to start, P to pause, R to reset, &larr;&rarr; to move, SPACE to shoot bullet"


pauseMessage : String
pauseMessage =
    "PAUSE"


overMessage : String
overMessage =
    "Invaders reached " ++ toString gameOverInvaders ++ "! Game over!"


make : { a | x : Float, y : Float } -> Shape -> Form
make obj shape =
    shape
        |> filled white
        |> move ( obj.x, obj.y )


makeSpaceship : { a | x : Float, y : Float } -> Form
makeSpaceship obj =
    image 40 40 "src/res/lambdaSpaceshuttle0.png"
        |> toForm
        |> move ( obj.x, obj.y + 20 )


makeBullet : { a | x : Float, y : Float } -> Form
makeBullet obj =
    image 12 30 "src/res/missile.png"
        |> toForm
        |> move ( obj.x, obj.y + 40 )


makeInvader : { a | x : Float, y : Float } -> Form
makeInvader obj =
    image 40 40 "src/res/javaInvader0.png"
        |> toForm
        |> move ( obj.x, obj.y )
