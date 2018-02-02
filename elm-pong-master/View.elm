module View exposing (..)

import Model exposing (..)
import Color exposing (..)
import Collage exposing (..)
import Text
import Element exposing (..)


messageStatus : State -> Element
messageStatus state =
    case state of
        Play ->
            txt identity ""

        Pause ->
            txt identity pauseMessage

        Start ->
            txt identity startMessage


titleStatus : State -> Element
titleStatus state =
    case state of
        Play ->
            txt identity ""

        Pause ->
            txt identity ""

        Start ->
            image 400 340 "../res/mainTitle.png"



--statusScore : State -> Element
--statusTitle state =
--    case state of
--        Play ->
--            txt identity ""
--        Pause ->
--            image 400 340 "../res/mainTitle.png"


verticalLine : Float -> Path
verticalLine height =
    path [ ( 0, height ), ( 0, -height ) ]


blackBackground : Color
blackBackground =
    rgb 0 0 0


txt : (Text.Text -> Text.Text) -> String -> Element
txt f =
    Text.fromString >> Text.color yellow >> Text.monospace >> f >> leftAligned


startMessage : String
startMessage =
    "S to start, P to pause, R to reset, &larr;&rarr; to move, SPACE to shoot bullet"


pauseMessage : String
pauseMessage =
    "PAUSE"


make : { a | x : Float, y : Float } -> Shape -> Form
make obj shape =
    shape
        |> filled white
        |> move ( obj.x, obj.y )


makeSpaceship : { a | x : Float, y : Float } -> Form
makeSpaceship obj =
    image 40 40 "../res/lambdaSpaceshuttle0.png"
        |> toForm
        |> move ( obj.x, obj.y + 20 )


makeBullet : { a | x : Float, y : Float } -> Form
makeBullet obj =
    image 12 30 "../res/missile.png"
        |> toForm
        |> move ( obj.x, obj.y + 40 )


makeInvader : { a | x : Float, y : Float } -> Form
makeInvader obj =
    image 40 40 "../res/javaInvader0.png"
        |> toForm
        |> move ( obj.x, obj.y )
