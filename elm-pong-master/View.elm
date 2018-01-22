module View exposing (..)

import Model exposing (..)
import Color exposing (..)
import Collage exposing (..)
import Text
import Element exposing (..)


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
    "S to start, P to pause, R to reset, &larr;&rarr; to move, SPACE to shoot bullet"


make obj shape =
    shape
        |> filled white
        |> move ( obj.x, obj.y )
