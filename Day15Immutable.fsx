#load "./Helper.fsx"
open Helper

// Day 15 - Chiton
//
// Remarks : Basic Dijkstra shortest path algorithm. It's quite slow for part 2,
//           though I mostly blame that on the immutable data structures. They're
//           not a good choice for this solution

let isBottomRightPos (cavern : int [] []) (x, y) =
    (cavern.Length - 1 = y) && (cavern.[y].Length - 1 = x)

let tryValueAtPoint cavern (x, y) =
    cavern |> Array.tryItem y |> Option.bind (fun row -> row |> Array.tryItem x)

let adjacentPoints (x, y) = [| (x-1,y); (x+1, y); (x, y-1); (x,y+1) |]

let adjacentPointsAndValues cavern pt =
    adjacentPoints pt
    |> Array.choose (fun pt ->
        tryValueAtPoint cavern pt |> Option.map (mkTuple pt))

let rec dijkstra cavern cavernCosts visited (frontier : Set<int * (int*int)>) =
    // Set's in F# are sorted, so we can treat them like priority queues
    let cost, curr = frontier |> Set.minElement
    let frontier = Set.remove (cost, curr) frontier

    if isBottomRightPos cavern curr
    then cost // we've reached our goal
    else if Set.contains curr visited
    then dijkstra cavern cavernCosts visited frontier // we've already been here
    else

    let visited = Set.add curr visited

    // get the travel cost to the surrounding nodes
    let adjacent =
        adjacentPointsAndValues cavern curr
        |> Array.map (fun (pt, value) -> (pt, value + cost))
        |> Array.map (fun (pt, value) ->
            // use the existing cost if it's cheaper
            match Map.tryFind pt cavernCosts with
            | None -> pt, value
            | Some prevVal -> pt, min prevVal value)

    // update the total costs of the surrounding nodes
    let cavernCosts =
        (cavernCosts, adjacent)
        ||> Array.fold (fun cavernCosts (pt, value) ->
            Map.add pt value cavernCosts)

    let frontier =
        (frontier, adjacent)
        ||> Array.fold (fun frontier (pt, value) ->
            Set.add (value, pt) frontier)

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
    dijkstra cavern Map.empty Set.empty (Set [0, (0,0)])
    // Correct Answer: 462 took: 360ms

let part2 cavern =
    let cavern = expandCavern cavern
    dijkstra cavern Map.empty Set.empty (Set [0, (0,0)])
    // Correct Answer: 2846 took: 10527ms

Helper.measurePart1 part1 input
Helper.measurePart2 part2 input
