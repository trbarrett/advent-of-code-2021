#load "./Helper.fsx"

// Day 1 - Depth increasing: Simple windowing functions
//
// Part 1 - Check if the difference between numbers is increasing
// Part 2 - Check if the difference between groups of numbers is increasing


let part1 numbers =
    numbers |> Seq.pairwise |> Seq.filter (fun (x,y) -> y > x) |> Seq.length
    // Correct Answer: 1374, took: 2ms

let part2 numbers =
    numbers |> Seq.windowed 3 |> Seq.map Seq.sum
            |> Seq.pairwise |> Seq.filter (fun (x,y) -> y > x) |> Seq.length
    // Correct Answer: 1418, took: 2ms

let numbers =
    Helper.readLinesWithHashComments "day01.txt"
    |> Seq.map int |> List.ofSeq

Helper.measurePart1 part1 numbers
Helper.measurePart2 part2 numbers
