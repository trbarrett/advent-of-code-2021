#load "./Helper.fsx"
open Helper
open System.Collections.Generic

// Day 15 - Chiton
//
// Remarks : Basic Dijkstra shortest path algorithm. I'm using mutable data sets
//           in this version which makes it 2x faster, though it's still slower
//           than I'd hope.

let isBottomRightPos (cavern : int [] []) (x, y) =
    (cavern.Length - 1 = y) && (cavern.[y].Length - 1 = x)

let tryValueAtPoint cavern (x, y) =
    cavern |> Array.tryItem y |> Option.bind (fun row -> row |> Array.tryItem x)

let adjacentPoints (x, y) = [| (x-1,y); (x+1, y); (x, y-1); (x,y+1) |]

let adjacentPointsAndValues cavern pt =
    adjacentPoints pt
    |> Array.choose (fun pt ->
        tryValueAtPoint cavern pt |> Option.map (mkTuple pt))

type Frontier =
    { Contents : Set<int * int>
      Priority : Map<int, int * int> }

let rec dijkstra cavern
                 (cavernCosts : Dictionary<int *int, int>)
                 (visited : HashSet<int * int>)
                 (frontier : SortedSet<int * (int*int)>) =
    // Set's in F# are sorted, so we can treat them like priority queues
    let cost, curr = frontier.Min
    frontier.Remove (cost, curr) |> ignore

    if isBottomRightPos cavern curr
    then cost // we've reached our goal
    else if visited.Contains curr
    then dijkstra cavern cavernCosts visited frontier // we've already been here
    else

    visited.Add curr |> ignore

    // get the travel cost to the surrounding nodes
    let adjacent =
        adjacentPointsAndValues cavern curr
        |> Array.map (fun (pt, value) -> (pt, value + cost))

    // update the total costs of the surrounding nodes
    for pt, value in adjacent do
        match cavernCosts.ContainsKey pt with
        | false -> cavernCosts.Add(pt, value)
        | true ->
            let existingVal = cavernCosts.[pt]
            cavernCosts.[pt] <- min existingVal value

    for pt, value in adjacent do
        frontier.Add((value, pt)) |> ignore

    dijkstra cavern cavernCosts visited frontier

let add1sToRow row = row |> Array.map (fun x -> (x % 9) + 1)
let add1sToCavern cavern = cavern |> Array.map add1sToRow

let expandCavern cavern =
    let cavern1 = add1sToCavern cavern
    let cavern2 = add1sToCavern cavern1
    let cavern3 = add1sToCavern cavern2
    let cavern4 = add1sToCavern cavern3

    let highCavern = Array.concat [cavern; cavern1; cavern2; cavern3; cavern4]

    highCavern
    |> Array.map(fun row ->
        let row1 = add1sToRow row
        let row2 = add1sToRow row1
        let row3 = add1sToRow row2
        let row4 = add1sToRow row3
        Array.concat [row; row1; row2; row3; row4])

let input =
    readLinesWithHashComments "day15.txt"
    |> Seq.map (Seq.map Char.digitToInt >> Seq.toArray)
    |> Seq.toArray

let part1 cavern =
    dijkstra
        cavern
        (new Dictionary<int * int, int>())
        (new HashSet<(int * int)>())
        (new SortedSet<int * (int * int)>([0, (0,0)]))
    // Correct Answer: 462 took: 168ms

let part2 cavern =
    let cavern = expandCavern cavern
    dijkstra
        cavern
        (new Dictionary<int * int, int>())
        (new HashSet<(int * int)>())
        (new SortedSet<int * (int * int)>([0, (0,0)]))
    // Correct Answer: 2846 took: 4907ms

Helper.measurePart1 part1 input
Helper.measurePart2 part2 input
