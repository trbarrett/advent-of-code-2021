#load "./Helper.fsx"

open Helper

// Day 2 - Sub Depth: Simple movement instructions
//
// Part 1 - Simple addition
// Part 2 - Still simple arithmetic, but with a third factor

type SubVec = | Forward of int | Down of int | Up of int

module SubVec =
    let parse str =
        let [| dir; amount |] = String.split ' ' str
        let amount = int amount
        match dir with
        | "forward" -> Forward amount
        | "down" -> Down amount
        | "up" -> Up amount

    let addPart1 (x, y) = function
       | Forward n -> (x + n, y)
       | Down n -> (x, y + n)
       | Up n -> (x, y - n)

    let addPart2 (x, depth, aim) = function
       | Forward n -> (x + n, depth + (n * aim), aim)
       | Down n -> (x, depth, aim + n)
       | Up n -> (x, depth, aim - n)

let part1 directions =
    ((0,0), directions) ||> List.fold SubVec.addPart1 |> fun (x,y) -> x * y
    // Correct Answer: 2039256, took: 0ms

let part2 directions =
    ((0,0,0), directions) ||> List.fold SubVec.addPart2 |> fun (x,y, _) -> x * y
    // Correct Answer: 1856459736, took: 0ms

let directions =
    readLinesWithHashComments "day02.txt"
    |> List.ofSeq |> List.map SubVec.parse

Helper.measurePart1 part1 directions
Helper.measurePart2 part2 directions
