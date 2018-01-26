module GeneticHelper exposing (..)

import Genetic exposing (..)
import State exposing (..)
import Html exposing (..)
import List exposing (..)
import Model exposing (..)


initialValueState : ( List a, number )
initialValueState =
    State.run 0 (state [])



-- evolve : Population -> Population
-- spawnNewInvaders : Population -> Invaders


main : Html msg
main =
    initialValueState
        |> toString
        |> text
