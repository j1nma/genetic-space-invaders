module GeneticHelper exposing (..)

-- https://github.com/ckoster22/elm-genetic/blob/master/src/Genetic.elm
-- import State exposing (..)

import Genetic exposing (..)
import Random exposing (..)
import Tuple exposing (..)
import Html exposing (..)


{-| Order: [x direction change probability, y direction change probability, x velocity, y velocity]
-}
type alias Dna =
    { dna : List Float
    , fitness : Float
    }


{-| For simple use cases the genetic algorithm will be doing one of two things:

  - Maximizing a score
  - Minimizing a penalty or cost
    Your `evaluateSolution` function is used to assign a value to an entire generation of possible solutions. `Method` tells the algorithm whether to keep and "breed" the solutions with a higher value or a lower value.

-}
myOptions : Options Dna
myOptions =
    { randomDnaGenerator = randDnaGenerator
    , evaluateSolution = evaluateSolution
    , crossoverDnas = crossoverDnas
    , mutateDna = mutateDna
    , isDoneEvolving = isDoneEvolving
    , method = MinimizePenalty
    }


numberOfGenes : Int
numberOfGenes =
    4


indexWhereProbabilitiesEnd : Int
indexWhereProbabilitiesEnd =
    1



-- Evaluate solution


evaluateSolution : Dna -> Float
evaluateSolution dna =
    dna.fitness



-- Dna generator


randDnaGenerator : Generator Dna
randDnaGenerator =
    Random.float -100 100
        |> Random.list 2
        |> Random.map2 (++) randomProbabilityGenerator
        |> Random.map
            (\randDna ->
                { dna = randDna
                , fitness = 0.0
                }
            )


randomProbabilityGenerator : Generator (List Float)
randomProbabilityGenerator =
    Random.float 0 1
        |> Random.list 2


runDnaGenerator : Int -> Dna
runDnaGenerator time =
    Tuple.first (Random.step randDnaGenerator (Random.initialSeed time))


runMutatedDnaGenerator : Int -> Dna -> Dna
runMutatedDnaGenerator time dna =
    Tuple.first (Random.step (mutateDna dna) (Random.initialSeed time))



-- Crossover


crossover_split_index : Int
crossover_split_index =
    floor (toFloat numberOfGenes / 2)


crossoverDnas : Dna -> Dna -> Dna
crossoverDnas dna1 dna2 =
    let
        ( dnaPart1, dnaPart2 ) =
            ( List.take crossover_split_index dna1.dna, List.drop crossover_split_index dna2.dna )
    in
        { dna = List.append dnaPart1 dnaPart2
        , fitness = (dna1.fitness + dna2.fitness) / 2
        }



-- Mutation


mutateDna : Dna -> Generator Dna
mutateDna dna =
    let
        randomIndexGenerator =
            Random.int 0 (numberOfGenes - 1)

        randomVelGenerator =
            Random.float -100 100

        randomProbGenerator =
            Random.float 0 1
    in
        Random.map3
            (\randomIndex randomVelGene randomProbGene ->
                dna.dna
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
                    |> (\randDna ->
                            { dna = randDna
                            , fitness = dna.fitness
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


initialEvolve : Int -> IntermediateValue Dna
initialEvolve time =
    Tuple.first (Random.step (Genetic.executeInitialStep myOptions) (Random.initialSeed time))


initialDna : Int -> Dna
initialDna time =
    dnaFromValue (initialEvolve time)


evolve : Int -> IntermediateValue Dna -> IntermediateValue Dna
evolve time intermediate =
    (Tuple.first (Random.step (Genetic.executeStep myOptions intermediate) (Random.initialSeed time)))


main : Html msg
main =
    let
        first =
            initialEvolve 10

        second =
            evolve 12 first

        third =
            evolve 13 second
    in
        numGenerationsFromValue third
            |> toString
            |> (++) (toString (numGenerationsFromValue second))
            |> (++) (toString (numGenerationsFromValue first))
            |> text
