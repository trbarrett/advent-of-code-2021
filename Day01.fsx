#load "./Helper.fsx"

// Day 1 - Depth increasing: Simple windowing functions
//
// Part 1 - Check if the difference between numbers is increasing
// Part 2 - Check if the difference between groups of numbers is increasing

let numbers =
    Helper.readLinesWithHashComments "day01.txt"
    |> Seq.map int |> List.ofSeq

let part1 () =
    numbers |> Seq.pairwise |> Seq.filter (fun (x,y) -> y > x) |> Seq.length
    // Correct Answer: 1374, took: 2ms

let part2 () =
    numbers |> Seq.windowed 3 |> Seq.map Seq.sum
            |> Seq.pairwise |> Seq.filter (fun (x,y) -> y > x) |> Seq.length
    // Correct Answer: 1418, took: 2ms

Helper.measurePart1 part1
Helper.measurePart2 part2
