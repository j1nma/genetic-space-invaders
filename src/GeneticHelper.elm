module GeneticHelper exposing (..)

-- https://github.com/ckoster22/elm-genetic/blob/master/src/Genetic.elm
-- import State exposing (..)

import Genetic exposing (..)
import Random exposing (..)
import Tuple exposing (..)


type alias Genes =
    { vx : Float
    , vy : Float
    , xProbChange : Float
    , yProbChange : Float
    }


type alias Dna =
    { genes :
        Genes
    , fitness : Float
    }


numberOfGenes : Int
numberOfGenes =
    4


myOptions : Options Dna
myOptions =
    { randomDnaGenerator = randDnaGenerator
    , evaluateSolution = evaluateSolution
    , crossoverDnas = crossoverDnas
    , mutateDna = mutateDna
    , isDoneEvolving = isDoneEvolving
    , method = MaximizeScore
    }


invaderVelocity : Float
invaderVelocity =
    150.0



-- Evaluate solution


evaluateSolution : Dna -> Float
evaluateSolution dna =
    dna.fitness



-- Dna generator


randGeneGenerator : Generator Genes
randGeneGenerator =
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


randDnaGenerator : Generator Dna
randDnaGenerator =
    Random.map
        (\randGenes ->
            { genes = randGenes
            , fitness = 0.0
            }
        )
        randGeneGenerator



-- Crossover


crossoverDnas : Dna -> Dna -> Dna
crossoverDnas dna1 dna2 =
    { genes =
        { vx = dna1.genes.vx
        , vy = dna2.genes.vy
        , xProbChange = dna1.genes.xProbChange
        , yProbChange = dna2.genes.yProbChange
        }
    , fitness = (dna1.fitness + dna2.fitness) / 2
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
                    genes =
                        dna.genes

                    mutatedGenes =
                        case randomIndex of
                            0 ->
                                { genes | vx = randomVelGene }

                            1 ->
                                { genes | vy = randomVelGene }

                            2 ->
                                { genes | xProbChange = randomProbGene }

                            3 ->
                                { genes | yProbChange = randomProbGene }

                            otherwise ->
                                genes
                in
                    { genes = mutatedGenes
                    , fitness = dna.fitness
                    }
            )
            randomIndexGenerator
            randomVelGenerator
            randomProbGenerator



-- Stop evolution


max_iterations : Int
max_iterations =
    3000


isDoneEvolving : Dna -> Float -> Int -> Bool
isDoneEvolving bestDna bestDnaScore numGenerations =
    bestDnaScore == toFloat Random.maxInt || numGenerations >= max_iterations


initialEvolve : Seed -> ( IntermediateValue Dna, Seed )
initialEvolve seed =
    Random.step (Genetic.executeInitialStep myOptions) seed


initialDna : Seed -> Dna
initialDna seed =
    dnaFromValue (Tuple.first (initialEvolve seed))


evolve : ( IntermediateValue Dna, Seed ) -> ( IntermediateValue Dna, Seed )
evolve ( intermediate, seed ) =
    Random.step (Genetic.executeStep myOptions intermediate) seed
