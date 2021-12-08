#load "./Helper.fsx"
open Helper

// Day 8 - Reverse engineering display inputs - Constraint propagation problem
//
// Parts 1 & 2 - Hand written constraint solver via sets
//
// Remarks:
//   We could have written a generalized constraint propagation solver, but the
//   solution space is simple enough I just hardcoded a solution

// Solve constraints to figure out what sets of segments match to which numbers
let determineNumberSets (signalPatterns : Set<Set<char>>) =
    let no1 = signalPatterns |> Seq.find (fun xs -> Set.count xs = 2)
    let no7 = signalPatterns |> Seq.find (fun xs -> Set.count xs = 3)
    let no4 = signalPatterns |> Seq.find (fun xs -> Set.count xs = 4)
    let no8 = signalPatterns |> Seq.find (fun xs -> Set.count xs = 7)

    let sixSegments = signalPatterns |> Set.filter (fun xs -> Set.count xs = 6)
    let fiveSegments = signalPatterns |> Set.filter (fun xs -> Set.count xs = 5)

    // #3 is the only number with 5 segments that is a subset of #7
    let no3 = fiveSegments |> Seq.find (fun xs -> Set.isSubset no7 xs )
    // #6 is the only number with 6 segments that doesn't contain #1 as a subset.
    let no6 = sixSegments |> Seq.find (fun xs -> Set.isSubset no1 xs |> not)
    // #5 is the only number with 5 segments that is a subset of #6
    let no5 = fiveSegments |> Seq.find (fun xs -> Set.isSubset xs no6)
    // #2 is the now the only number left with 5 segments
    let no2 = fiveSegments |> Seq.find (fun xs -> xs <> no5 && xs <> no3)
    // #9 = #1 union #5
    let no9 = sixSegments |> Seq.find (fun xs -> Set.union no1 no5 = xs)
    // #0 is the now the only number left with 6 segments
    let no0 = sixSegments |> Seq.find (fun xs -> xs <> no9 && xs <> no6)

    [ no0; no1; no2; no3; no4; no5; no6; no7; no8; no9 ]

let entries =
    readLinesWithHashComments "day08.txt" |> List.ofSeq
    |> List.map (fun str ->
        let [| signalPatterns; outputValues |] = String.split '|' str
        let signalPatterns = String.split ' ' signalPatterns |> Array.map Set |> Set
        let outputValues = String.split ' ' (outputValues |> String.trim) |> Array.map Set
        signalPatterns, outputValues)

let part1 entries =
    entries
    |> List.sumBy (fun (signalPatterns, outputValues) ->
        let numberSets = determineNumberSets signalPatterns
        outputValues
        |> Seq.map (fun input -> numberSets |> List.findIndex ((=) input))
        |> Seq.filter (fun x -> List.contains x [1; 4; 7; 8])
        |> Seq.length)
    // Correct Answer: 554 took: 7ms

let part2 entries =
    entries
    |> List.sumBy (fun (signalPatterns, outputValues) ->
        let numberSets = determineNumberSets signalPatterns
        outputValues
        |> Seq.map (fun input -> numberSets |> List.findIndex ((=) input))
        |> Math.digitsToInt32)
    // Correct Answer: 990964 took: 12ms

part1 entries |> ignore // perform a cold run
Helper.measurePart1 part1 entries
Helper.measurePart2 part2 entries
