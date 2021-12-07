#load "./Helper.fsx"
open Helper

// Day 7 - Aligning crabs - Simple optimization problem
//
// Part 1 - Binary search for lowest cost
// Part 2 - Same thing, but with a non-linear calculation

let costCalculationLinear targetPos positionCounts =
    positionCounts
    |> List.sumBy (fun (pos, count) -> abs (targetPos - pos) * count)

let costCalculationSeriesSum targetPos positionCounts =
    let seriesSum x = (x * (x + 1)) / 2
    positionCounts
    |> List.sumBy (fun (pos, count) ->
        seriesSum (abs (targetPos - pos)) * count)

let rec binarySearch costCalc minPos maxPos positionCounts =
    if minPos = maxPos
    then costCalc minPos positionCounts
    else
        let testPoint = ((maxPos - minPos) / 2) + minPos
        // we use 2 test calculations to see which direction the cost is going
        let testCost = costCalc testPoint positionCounts
        let testCost' = costCalc (testPoint + 1) positionCounts
        if testCost <= testCost'
        then binarySearch costCalc minPos testPoint positionCounts
        else binarySearch costCalc (testPoint + 1) maxPos positionCounts

let calculateOptimalFuelUsage costCalc (crabPositions : int list) =
    let minPos = List.min crabPositions
    let maxPos = List.max crabPositions
    let positionCounts = crabPositions |> List.countBy id
    binarySearch costCalc minPos maxPos positionCounts

let crabPositions =
    readLinesWithHashComments "day07.txt" |> Seq.item 0
    |> String.split ',' |> Seq.map int |> Seq.toList

let part1 crabPositions =
    calculateOptimalFuelUsage costCalculationLinear crabPositions
    // Correct Answer: 349769 took: 0ms

let part2 crabPositions =
    calculateOptimalFuelUsage costCalculationSeriesSum crabPositions
    // Correct Answer: 99540554 took: 0ms

part1 crabPositions |> ignore // perform a cold run
Helper.measurePart1 part1 crabPositions
Helper.measurePart2 part2 crabPositions
