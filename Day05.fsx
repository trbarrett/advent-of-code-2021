#load "./Helper.fsx"
open Helper

// Day 5 - Hydrothermal vents:
//
// Part 1 - Find the positions where *axial* lines cross
// Part 2 - Find the positions where *all* the lines cross
//          (finding the integer points on an arbitrary line)
//
// Remarks: This is quite slow, and I imagine that's entirely due to using an
//          immutable map as the data type. Changing it to a mutable dictionary
//          would be an easy fix, so I'm not that worried

type Line = { From: int * int; To: int * int }
module Line =
    let parse str =
        let [ x1; y1; x2; y2 ] = String.splitIntoMatching "\d+" str
        { From = int x1, int y1; To = int x2, int y2 }

    let isAxial { From = x1, y1; To = x2, y2 } =
       (x1 = x2) || (y1 = y2)

type Graph = Map<int * int, int>
module Graph =
    let empty : Graph = Map.empty

    let recordVentPoint graph point =
        graph
        |> Map.change point (function | None -> Some 1
                                      | Some x -> Some (x + 1))
    let getPoints { From = x1, y1; To = x2, y2 } =
       let steps = max (abs (x2 - x1)) (abs (y2 - y1))
       let xStepSize = (float (x2 - x1)) / (float steps)
       let yStepSize = (float (y2 - y1)) / (float steps)
       [ for step in 0..steps -> (x1 + int (float step * xStepSize),
                                  y1 + int (float step * yStepSize))]

    let addLinePoints graph line =
        getPoints line
        |> List.fold recordVentPoint graph

let lines =
    readLinesWithHashComments "day05.txt" |> List.ofSeq
    |> List.map Line.parse

let part1 lines =
    let graph =
        lines
        |> List.filter Line.isAxial
        |> List.fold Graph.addLinePoints Graph.empty

    graph |> Map.toList |> List.filter (fun (_, n) -> n > 1) |> List.length
    // Correct Answer: 5169 took: 428ms

let part2 lines =
    let graph = lines |> List.fold Graph.addLinePoints Graph.empty
    graph |> Map.toList |> List.filter (fun (_, n) -> n > 1) |> List.length
    // Correct Answer: 22083 took: 841ms

Helper.measurePart1 part1 lines
Helper.measurePart2 part2 lines
