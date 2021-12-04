#load "./Helper.fsx"

open Helper

// Day 2 - Binary Diagnostic: Simple list counting
//
// Part 1 - Count the number of ones in each column
// Part 2 - Same as part 1, but with a filtering step
//
// Remarks: There's probably fancier and faster ways of doing this with bit
//          fiddling, but this

let binaryNumbers =
    readLinesWithHashComments "day03.txt"
    |> List.ofSeq |> List.map (Seq.map Char.digitToInt >> List.ofSeq)

let part1 () =
    // count up the `1`s in each column
    let onesCounts =
        binaryNumbers
        |> List.reduce (fun xs ys -> [ for x, y in List.zip xs ys -> x + y ])

    let gamma =
        onesCounts
        // set a 1 or 0 depending on what's the most common in each column
        |> List.map (fun x -> if x > (List.length binaryNumbers / 2) then 1 else 0)
        // and convert the resulting binary representation to an int32
        |> Math.binaryDigitsToInt32

    let epsilon = (pown 2 (List.length onesCounts) - 1) - gamma

    gamma * epsilon
    // Correct Answer: 2003336 took: 5ms

let getOnesCountInColumn (binaryNumbers : int list list) column =
    binaryNumbers |> List.map (List.item column) |> List.sum

let rec whittleDownBinaries determineWhitter binaryNumbers column =
    match binaryNumbers with
    | [ singleResult ] -> singleResult
    | xs ->
        let onesCount = getOnesCountInColumn xs column
        let whittleItem = determineWhitter onesCount (List.length xs)
        let remaining = xs |> List.filter (fun x -> List.item column x = whittleItem)
        whittleDownBinaries determineWhitter remaining (column + 1)

let part2 () =
    let matchMostCommon onesCount len = if onesCount * 2 >= len then 1 else 0
    let matchLeastCommon onesCount len = if onesCount * 2 >= len then 0 else 1

    let oxygenGeneratorRating = whittleDownBinaries matchMostCommon binaryNumbers 0
    let co2ScrubberRating = whittleDownBinaries matchLeastCommon binaryNumbers 0

    (Math.binaryDigitsToInt32 oxygenGeneratorRating)
    * (Math.binaryDigitsToInt32 co2ScrubberRating)
    // Correct Answer: 1877139 took: 1ms

Helper.measurePart1 part1
Helper.measurePart2 part2
