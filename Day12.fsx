#load "./Helper.fsx"
open Helper

// Day 12 - Passage Pathing - Graph traversal variation
//
// Part 1 - Depth first search, except we can revisit Big caves
// Part 2 - Same as part 1, except can revisit a single Small cave once
//
// Remarks: Part 1 is a relatively straight forwards DFS. We just don't remember
//          that we've visited a Big cave.
//          Part 2 complicates things a little bit, but we just keep an extra
//          bool to remember if we've visited a small cave.
//
//          Performance for part 2 isn't great. A little over 1 second. Putting
//          the caves into a Map is an obvious potential performance improvement

type Cave = | BigCave of string | SmallCave of string

let findPaths canVisitSmallTwice (caves : (Cave * Cave) list) =
    let rec dfs (caves : (Cave * Cave) list) curr visited visitSmallTwice path =
        match visited |> Set.contains curr, visitSmallTwice, curr with
        | _, _, SmallCave "end" -> [path]
        | true, true, SmallCave _ when curr <> SmallCave "start" ->
            let moves = caves |> List.choose (fun (x,y) -> if x = curr then Some y else None)
            moves |> List.collect (fun x -> dfs caves x visited false (x::path))
        | false, visitSmallTwice, _ ->
            let visited =
                match curr with
                | BigCave _ -> visited
                | SmallCave _ -> visited |> Set.add curr
            let moves = caves |> List.choose (fun (x,y) -> if x = curr then Some y else None)
            moves |> List.collect (fun x -> dfs caves x visited visitSmallTwice (x::path))
        | true, _, _ -> []

    let startCave = SmallCave "start"
    dfs caves startCave Set.empty canVisitSmallTwice [startCave]

let mkCave (name : string) =
    if System.Char.IsUpper name.[0]
    then BigCave name else SmallCave name

let caves =
    readLinesWithHashComments "day12.txt" |> List.ofSeq
    |> List.map (fun line ->
        let [| caveNameA; caveNameB |] = String.split '-' line
        mkCave caveNameA, mkCave caveNameB)
    |> List.collect (fun (caveA, caveB) -> [caveA, caveB; caveB, caveA])

let part1 caves =
    caves |> findPaths false |> List.length
    // Correct Answer: 5254 took: 146ms

let part2 caves =
    caves |> findPaths true |> List.length
    // Correct Answer: 149385 took: 1167ms

Helper.measurePart1 part1 caves
Helper.measurePart2 part2 caves
