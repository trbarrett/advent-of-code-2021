#load "./Helper.fsx"
open Helper

// Day 11 - Dumbo Octopuses - Game of life variation
//                            (mini GoL (for flashes) within a GoL)
//
// Part 1 - Count the total flashes in 100 steps
// Part 2 - Find when everything flashes at once
//
// Remarks: The hard part was getting the flashes working consistently and not
//          double counting things. Took me a while to figure out the correct
//          solution. Originally I had the increment and flashes as part of the
//          same function, but that ran into trouble when it had to recurse.
//          Once I separated them the rest came together pretty quickly
//
//          It's slower than I like. But to keep things immutable and simple I'm
//          iterating through all the items extra times.

let tryValueAtPoint cavern (x, y) =
    cavern |> Array.tryItem y |> Option.bind (fun row -> row |> Array.tryItem x)

let adjacentPoints (x, y) =
    [| (x-1,y-1); (x,y-1); (x+1, y-1)
       (x-1,y);            (x+1, y);
       (x-1,y+1); (x,y+1); (x+1, y+1); |]

let adjacentValues cavern pt =
    adjacentPoints pt |> Array.choose (tryValueAtPoint cavern)

let incEnergyBy1 cavern =
    cavern |> Array.map (Array.map (fun value -> value + 1))

let rec flash (cavern, flashCount) =
    let newFlashes = cavern |> Array.sumBy (Array.filter (fun x -> x > 9) >> Array.length)

    let cavern' =
        cavern
        |> Array.mapi (fun y row ->
            row |> Array.mapi (fun x value ->
                match value with
                | 0 -> 0
                | x when x > 9 -> 0
                | value ->
                    value
                    + (adjacentValues cavern (x,y)
                       |> Array.filter (fun x -> x > 9)
                       |> Array.length)))

    if cavern' <> cavern
    then flash (cavern', flashCount + newFlashes)
    else (cavern', flashCount + newFlashes)

let step (cavern, flashCount) =
    flash (incEnergyBy1 cavern, flashCount)

let cavern =
    let lines = readLinesWithHashComments "day11.txt" |> Array.ofSeq
    lines |> Array.map (Seq.map Char.digitToInt >> Array.ofSeq)

let part1 cavern =
    (cavern, 0)
    |> (Seq.replicate 100 step |> Seq.reduce (>>))
    |> snd
    // Correct Answer: 1732 took: 34ms

let part2 cavern =
    let cavernSize = cavern |> Array.sumBy Array.length
    let rec part2Loop cavern stepNo =
        let cavern, flashCount = step (cavern, 0)
        if flashCount = cavernSize
        then stepNo
        else part2Loop cavern (stepNo + 1)
    part2Loop cavern 1
    // Correct Answer: 290 took: 59ms

Helper.measurePart1 part1 cavern
Helper.measurePart2 part2 cavern