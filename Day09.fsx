#load "./Helper.fsx"
open Helper

// Day 9 - Lavatubes - Finding the smallest number compared to neighbours
//
// Part 1 - Scan through the array looking for valid points
// Part 2 - Breadth first search to find size of basin

type Heightmap = { Width : int; Rows : int [] [] }

module Heightmap =
    let valueAtPoint { Rows = rows } (x, y) = rows.[y].[x]

    let tryValueAtPoint { Rows = rows } (x, y) =
        rows |> Array.tryItem y |> Option.bind (fun row -> row |> Array.tryItem x)

    let adjacentPoints (x, y) = [| (x-1,y); (x+1, y); (x, y-1); (x,y+1) |]

    let adjacentValues heightmap pt =
        adjacentPoints pt |> Array.choose (tryValueAtPoint heightmap)

    let adjacentPointsAndValues heightmap pt =
        adjacentPoints pt
        |> Array.choose (fun pt ->
            tryValueAtPoint heightmap pt |> Option.map (mkTuple pt))

    let allPoints heightmap =
        [| for x in [0..(heightmap.Width - 1)] do
               for y in [0..(heightmap.Rows.Length - 1)] -> (x,y) |]

let findBasinSize heightmap pt =
    let rec bfs visited frontier =
        match frontier with
        | [] -> Set.count visited
        | curr::rest when Set.contains curr visited -> bfs visited rest
        | curr::rest ->
            let currValue = Heightmap.valueAtPoint heightmap curr
            let adjacent =
                Heightmap.adjacentPointsAndValues heightmap curr
                |> Array.filter (fun (_, value) -> value > currValue && value <> 9)
                |> Array.unzip |> fst |> Array.toList
            bfs (Set.add curr visited) (rest@adjacent)
    bfs Set.empty [ pt ]

let lowestPoints heightmap =
    Heightmap.allPoints heightmap
    |> Array.filter (fun pt ->
        let value = Heightmap.valueAtPoint heightmap pt
        Heightmap.adjacentValues heightmap pt
        |> Array.forall (fun x -> x > value))

let heightmap =
    let lines = readLinesWithHashComments "day09.txt" |> Array.ofSeq
    { Width = lines.[0].Length
      Rows = lines |> Array.map (Seq.map Char.digitToInt >> Array.ofSeq) }

let part1 heightmap =
    lowestPoints heightmap
    |> Array.sumBy (Heightmap.valueAtPoint heightmap >> ((+) 1))
    // Correct Answer: 516 took: 3ms

let part2 heightmap =
    lowestPoints heightmap
    |> Array.map (findBasinSize heightmap)
    |> Array.sortDescending |> Array.take 3 |> Array.reduce (*)
    // Correct Answer: 1023660 took: 22ms

part2 heightmap |> ignore // perform a cold run
Helper.measurePart1 part1 heightmap
Helper.measurePart2 part2 heightmap
