module GeneticHelper exposing (..)

import Genetic exposing (..)
import Random exposing (..)
import Tuple exposing (..)


type alias Dna =
    { vx : Float
    , vy : Float
    , xProbChange : Float
    , yProbChange : Float
    }


numberOfGenes : Int
numberOfGenes =
    4


options : (Dna -> Float) -> Options Dna
options evaluateSolution =
    { randomDnaGenerator = randDnaGenerator
    , evaluateSolution = evaluateSolution
    , crossoverDnas = crossoverDnas
    , mutateDna = mutateDna
    , isDoneEvolving = (\_ _ _ -> False)
    , method = MaximizeScore
    }


invaderVelocity : Float
invaderVelocity =
    150.0



-- Dna generator


randDnaGenerator : Generator Dna
randDnaGenerator =
    Random.map4
        (\randXVel randYVel randXProb randYProb ->
            { vx = randXVel
            , vy = randYVel
            , xProbChange = randXProb
            , yProbChange = randYProb
            }
        )
        (Random.float -invaderVelocity invaderVelocity)
        (Random.float -invaderVelocity invaderVelocity)
        (Random.float 0 1)
        (Random.float 0 1)



-- Crossover


crossoverDnas : Dna -> Dna -> Dna
crossoverDnas dna1 dna2 =
    { vx = dna1.vx
    , vy = dna2.vy
    , xProbChange = dna1.xProbChange
    , yProbChange = dna2.yProbChange
    }



-- Mutation


mutateDna : Dna -> Generator Dna
mutateDna dna =
    let
        randomIndexGenerator =
            Random.int 0 (numberOfGenes - 1)

        randomVelGenerator =
            Random.float -invaderVelocity invaderVelocity

        randomProbGenerator =
            Random.float 0 1
    in
        Random.map3
            (\randomIndex randomVelGene randomProbGene ->
                let
                    mutatedDna =
                        case randomIndex of
                            0 ->
                                { dna | vx = randomVelGene }

                            1 ->
                                { dna | vy = randomVelGene }

                            2 ->
                                { dna | xProbChange = randomProbGene }

                            3 ->
                                { dna | yProbChange = randomProbGene }

                            _ ->
                                dna
                in
                    mutatedDna
            )
            randomIndexGenerator
            randomVelGenerator
            randomProbGenerator


initialEvolve : Seed -> ( IntermediateValue Dna, Seed )
initialEvolve seed =
    Random.step (Genetic.executeInitialStep (options (\_ -> 0.0))) seed


initialDna : Seed -> Dna
initialDna seed =
    dnaFromValue (Tuple.first (initialEvolve seed))


evolve : (Dna -> Float) -> ( IntermediateValue Dna, Seed ) -> ( IntermediateValue Dna, Seed )
evolve evaluateSolution ( intermediate, seed ) =
    Random.step (Genetic.executeStep (options evaluateSolution) intermediate) seed
