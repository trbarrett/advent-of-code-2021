#load "./Helper.fsx"
open Helper

// Day 6 - Lanternfish
//
// Part 1 - Exponential growth
// Part 2 - Same, but larger time period
//
// Remarks - Used a buffer ring to keep track of fish on each day, with a
//           separate buffer for baby fish. Part 2 required using 64 bit ints

type FishState =
    { mutable Day : int
      AdultFish : int64 array
      BabyFish : int64 array }

module FishState =

    let fill breedingDays =
        let state =
            { Day = 0
              AdultFish = Array.replicate 7 0L
              BabyFish = Array.replicate 7 0L }
        breedingDays |> Seq.iter (fun day ->
            state.AdultFish.[day] <- state.AdultFish.[day] + 1L)
        state

    let nextDay state =
        state.BabyFish.[(state.Day + 2) % 7] <- state.AdultFish.[state.Day]
        state.AdultFish.[state.Day] <- state.AdultFish.[state.Day] + state.BabyFish.[state.Day]
        state.BabyFish.[state.Day] <- 0L
        state.Day <- (state.Day + 1) % 7
        state

let calculateFishPopulation startingFish days =
    let mutable state = FishState.fill startingFish
    for i = 1 to days do state <- FishState.nextDay state
    (state.AdultFish |> Array.sum) + (state.BabyFish |> Array.sum)

let startingFish =
    readLinesWithHashComments "day06.txt" |> Seq.item 0
    |> String.split ',' |> Seq.map int |> Seq.toList

let part1 startingFish =
    calculateFishPopulation startingFish 80
    // Correct Answer: 353079 took: 0ms

let part2 startingFish =
    calculateFishPopulation startingFish 256
    // Correct Answer: 1605400130036 took: 0ms

part1 startingFish |> ignore // perform a cold run

Helper.measurePart1 part1 startingFish
Helper.measurePart2 part2 startingFish
