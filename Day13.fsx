#load "./Helper.fsx"
open Helper

// Day 12 - Transparent Origami - Folding paper
//
// Day 1 - Simple maths to fold points on paper represented by a set
// Day 2 - Print a sparce Set structure representing points on graph, by finding
//         the extents then looping through every possible point

type Fold = | X of int | Y of int

let doNextFold points fold =
    // Slowly create a new map of points by going through each existing point
    // and finding where they would be on the new map
    (Set.empty, points)
    ||> Set.fold (fun s (x,y) ->
        let x, y =
            match fold with
            | X fx -> (if x <= fx then x else 2 * fx - x), y
            | Y fy -> x, (if y <= fy then y else 2 * fy - y)
        Set.add (x,y) s)

let getExtents paper =
    let startExtent = (System.Int32.MaxValue, System.Int32.MinValue,
                       System.Int32.MaxValue, System.Int32.MinValue)
    (startExtent, paper)
    ||> Set.fold (fun (minX, maxX, minY, maxY) (x,y) ->
        (if x < minX then x else minX), (if x > maxX then x else maxX),
        (if y < minY then y else minY), (if y > maxY then y else maxY))

let printPaper paper =
    let minX, maxX, minY, maxY = getExtents paper
    let paper =
        [ for y in minY..maxY ->
            [for x in minX..maxX ->
                match Set.contains (x,y) paper with
                | false -> " " | true -> "#"]]
    printfn "\n%s\n" (Seq.toString "\n" (paper |> List.map (Seq.toString "")))

let input =
    let [ points; folds ] =
        readLinesWithHashComments "day13.txt" |> splitOnEmptyLines
    let points =
        points |> List.map (String.split ',' >> Seq.toList >> (fun [x; y] -> (int x, int y)))
    let folds =
        folds
        |> List.map (String.capture "([xy])=(\d+)")
        |> List.map (fun [axis; amount] ->
            match axis with
            | "x" -> X (int amount)
            | "y" -> Y (int  amount))
    (points |> Set, folds)

let part1 (points, folds) =
    let folded = doNextFold points (List.head folds)
    Set.count folded
    // Correct Answer: 724 took: 5ms

let part2 (points, folds) =
    (points, folds) ||> List.fold (doNextFold) |> printPaper
    "'See Above'"
    // Correct Answer: CPJBERUL took: 14ms

Helper.measurePart1 part1 input
Helper.measurePart2 part2 input
