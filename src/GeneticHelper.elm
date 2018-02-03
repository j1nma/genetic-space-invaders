module GeneticHelper exposing (..)

-- https://github.com/ckoster22/elm-genetic/blob/master/src/Genetic.elm
-- import State exposing (..)

import Genetic exposing (..)
import Random exposing (..)
import Tuple exposing (..)


{-| Order: [x direction change probability, y direction change probability, x velocity, y velocity]
-}
type alias Dna =
    { genes : List Float
    , fitness : Float
    }


myOptions : Options Dna
myOptions =
    { randomDnaGenerator = randDnaGenerator
    , evaluateSolution = evaluateSolution
    , crossoverDnas = crossoverDnas
    , mutateDna = mutateDna
    , isDoneEvolving = isDoneEvolving
    , method = MaximizeScore
    }


numberOfGenes : Int
numberOfGenes =
    4


indexWhereProbabilitiesEnd : Int
indexWhereProbabilitiesEnd =
    1


invaderVelocity : Float
invaderVelocity =
    150.0



-- Evaluate solution


evaluateSolution : Dna -> Float
evaluateSolution dna =
    dna.fitness



-- Dna generator


randDnaGenerator : Generator Dna
randDnaGenerator =
    Random.float -invaderVelocity invaderVelocity
        |> Random.list 2
        |> Random.map2 (++) randomProbabilityGenerator
        |> Random.map
            (\randGenes ->
                { genes = randGenes
                , fitness = 0.0
                }
            )


randomProbabilityGenerator : Generator (List Float)
randomProbabilityGenerator =
    Random.float 0 1
        |> Random.list 2



-- Crossover


crossover_split_index : Int
crossover_split_index =
    floor (toFloat numberOfGenes / 2)


crossoverDnas : Dna -> Dna -> Dna
crossoverDnas dna1 dna2 =
    let
        ( dnaPart1, dnaPart2 ) =
            ( List.take crossover_split_index dna1.genes, List.drop crossover_split_index dna2.genes )
    in
        { genes = List.append dnaPart1 dnaPart2
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
                dna.genes
                    |> List.indexedMap
                        (\index gene ->
                            if index == randomIndex then
                                if index >= indexWhereProbabilitiesEnd then
                                    randomVelGene
                                else
                                    randomProbGene
                            else
                                gene
                        )
                    |> (\randGenes ->
                            { genes = randGenes
                            , fitness = dna.fitness + 1.0
                            }
                       )
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


evolve : Seed -> IntermediateValue Dna -> ( IntermediateValue Dna, Seed )
evolve seed intermediate =
    Random.step (Genetic.executeStep myOptions intermediate) seed
