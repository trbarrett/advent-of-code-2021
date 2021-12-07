#load "./Helper.fsx"
open Helper

// Day 6 - Lanternfish
//
// Part 1 - Exponential growth
// Part 2 - Same, but larger time period
//
// Remarks - Used a buffer ring to keep track of fish on each day, with a
//           separate buffer for baby fish. Part 2 required using 64 bit ints
//
//           This version is immutable, but that just meant copying the buffer
//           ring each time. It wasn't a large change or performance impact

type FishState =
    { Day : int
      AdultFish : int64 list
      BabyFish : int64 list }

module FishState =
    let fill breedingDays =
        let breedingDaysCount =
            breedingDays |> Seq.countBy id |> Map |> Map.mapValues int64

        { Day = 0
          AdultFish =
            [ 0..6 ] |> List.map (fun i ->
                breedingDaysCount |> Map.tryFind i |> Option.defaultValue 0L)
          BabyFish = List.replicate 7 0L }

    let nextDay state =
        let babyFish =
            state.BabyFish
            |> List.replaceAt ((state.Day + 2) % 7) state.AdultFish.[state.Day]
        let newAdultFishCount = state.AdultFish.[state.Day] + state.BabyFish.[state.Day]
        let adultFish = state.AdultFish |> List.replaceAt state.Day newAdultFishCount

        { Day = (state.Day + 1) % 7
          AdultFish = adultFish
          BabyFish = babyFish |> List.replaceAt state.Day 0L }

let calculateFishPopulation startingFish days =
    let mutable state = FishState.fill startingFish
    for i = 1 to days do state <- FishState.nextDay state
    (state.AdultFish |> List.sum) + (state.BabyFish |> List.sum)

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
