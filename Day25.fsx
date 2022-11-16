#load "./Helper.fsx"
open Helper

// Day 25: Sea Cucumber
//
// We have a grid of creatures that move semi-simultaneously and can block each
// other. The grid wraps around east-west and north-south like a torus. After
// enough steps the creatures will become gridlocked and be unable to move any
// more. The objective is to find out haw many steps that takes
//
// Approach:
//   The grid of creatures can be quite large, but we don't need to check and
//   update each position in the grid, only the creatures that are currently
//   unblocked. We keep two lists of the creatures that are unblocked (depending
//   on the direction of travel) and step through each list to find out their
//   new positions, and what around them may have become blocked or unblocked.
//
//   The list of creatures that are unblocked becomes smaller the more steps we
//   take and the closer the grid gets to becoming gridlocked. So even though
//   the calculations at the start take a long time, the closer we get to an
//   answer the lest options we need to consider and the faster it becomes.
//
//   It's not quite as fast as I would like at just under 2 seconds. But
//   perfectly adequate for a first attempt so I'll just leave it there.

type Cucumber = RightMove | DownMove | Empty

type CucumberMap = Cucumber[][]

type CucumbersState =
    { Map : CucumberMap
      RightUnlocked : Set<int * int>
      DownUnlocked : Set<int * int> }

let printMap (map : CucumberMap) =
    map |> Array.iter (fun row ->
        let rowStr =
            row |> Array.map (fun item ->
                match item with
                | Empty -> '.'
                | RightMove -> '>'
                | DownMove -> 'v')
        printfn "%s" (String.fromChars rowStr))

let parseInput lines =
    lines
    |> Seq.map(fun line ->
        line
        |> Seq.map (fun ch ->
            match ch with
            | 'v' -> DownMove
            | '>' -> RightMove
            | '.' -> Empty
            | _ -> failwithf "Unexpected input %c" ch )
        |> Seq.toArray)
    |> Seq.toArray

let wrappedPos cucumberMap (rowNo, columnNo) =
   let rowNo = (rowNo + Array.length cucumberMap) % (Array.length cucumberMap)
   let row = cucumberMap.[rowNo]
   let columnNo = (columnNo + Array.length row) % (Array.length row)
   (rowNo, columnNo)

let atPos cucumberMap (rowNo, columnNo) =
   let rowNo, columnNo = wrappedPos cucumberMap (rowNo, columnNo)
   cucumberMap.[rowNo].[columnNo]

let getInitialUnlocked cucumberMap =
    let foldItem (rightUnlocked, downUnlocked) (rowNo, columnNo, cucumber) =
        match cucumber with
        | Empty -> (rightUnlocked, downUnlocked)
        | DownMove ->
            if atPos cucumberMap (rowNo + 1, columnNo) = Empty
            then rightUnlocked, (rowNo, columnNo)::downUnlocked
            else rightUnlocked, downUnlocked
        | RightMove ->
            if atPos cucumberMap (rowNo, columnNo + 1) = Empty
            then (rowNo, columnNo)::rightUnlocked, downUnlocked
            else rightUnlocked, downUnlocked

    let foldRow (rightUnlocked, downUnlocked) (rowNo, row) =
        let row =
            Array.indexed row
            |> Array.map (fun (columnNo, cucumber) -> (rowNo, columnNo, cucumber))
        Array.fold foldItem (rightUnlocked, downUnlocked) row

    Array.fold foldRow ([],[]) (Array.indexed cucumberMap)

let newlyUnlocked unlock map =
    unlock
    |> Seq.map (fun (rowNo, columnNo) ->
        let rightUnlocked =
            match atPos map (rowNo, columnNo - 1) with
            | RightMove -> [wrappedPos map (rowNo, columnNo - 1)]
            | _ -> []
        let downUnlocked =
            match atPos map (rowNo - 1, columnNo) with
            | DownMove -> [wrappedPos map (rowNo - 1, columnNo)]
            | _ -> []
        (rightUnlocked, downUnlocked))
    |> Seq.toList
    |> List.unzip
    |> fun (rights, downs) -> List.concat rights, List.concat downs

let processRightStep (state : CucumbersState) =
    // find all the positions that would become unlocked after moving unlocked
    // to the right
    let newlyRightUnlocked, newlyDownUnlocked =
        newlyUnlocked state.RightUnlocked state.Map

    // move all the unlocked right to the right, and clear the current space
    let nextMap = state.Map |> Array.copy
    state.RightUnlocked
    |> Set.iter (fun (rowNo, columnNo) ->
        let nextRowNo, nextColumnNo = wrappedPos state.Map (rowNo, columnNo + 1)
        nextMap[nextRowNo][nextColumnNo] <- RightMove
        nextMap[rowNo][columnNo] <- Empty)

    // find which of the items we moved are going to remain unlocked
    let continueToBeUnlocked =
        state.RightUnlocked
        |> Seq.choose (fun (rowNo, columnNo) ->
            match atPos nextMap (rowNo, columnNo + 2) with
            | Empty -> Some (wrappedPos nextMap (rowNo, columnNo + 1))
            | _ -> None)

    // check what items above may now become locked after the move
    let downUnlocked =
        (state.DownUnlocked, state.RightUnlocked)
        ||> Set.fold (fun downUnlocked (rowNo, columnNo) ->
            Set.remove (wrappedPos state.Map (rowNo - 1, columnNo + 1)) downUnlocked)

    { Map = nextMap
      DownUnlocked = Set.union (Set newlyDownUnlocked) downUnlocked
      RightUnlocked = Set.union (Set newlyRightUnlocked) (Set continueToBeUnlocked) }


let processDownStep (state : CucumbersState) =
    // find all the positions that would become unlocked after moving unlocked
    // down.
    let newlyRightUnlocked, newlyDownUnlocked =
        newlyUnlocked state.DownUnlocked state.Map

    // move all the unlocked down, and clear the current space
    let nextMap = state.Map |> Array.copy
    state.DownUnlocked
    |> Set.iter (fun (rowNo, columnNo) ->
        let nextRowNo, nextColumnNo = wrappedPos state.Map (rowNo + 1, columnNo)
        nextMap[nextRowNo][nextColumnNo] <- DownMove
        nextMap[rowNo][columnNo] <- Empty)

    // find which of the items we moved are going to remain unlocked
    let continueToBeUnlocked =
        state.DownUnlocked
        |> Seq.choose (fun (rowNo, columnNo) ->
            match atPos nextMap (rowNo + 2, columnNo) with
            | Empty -> Some (wrappedPos nextMap (rowNo + 1, columnNo))
            | _ -> None)

    // check what items to the left may now become locked after the move
    let rightUnlocked =
        (state.RightUnlocked, state.DownUnlocked)
        ||> Set.fold (fun rightUnlocked (rowNo, columnNo) ->
            Set.remove (wrappedPos state.Map (rowNo + 1, columnNo - 1)) rightUnlocked)

    { Map = nextMap
      DownUnlocked = Set.union (Set newlyDownUnlocked) (Set continueToBeUnlocked)
      RightUnlocked = Set.union (Set newlyRightUnlocked) rightUnlocked }

let rec doStep (prevState : CucumbersState) n max =
    if n >= max
    then prevState, n
    else
        let state = processRightStep prevState
        let state = processDownStep state

        if state = prevState
        then state, n + 1
        else doStep state (n + 1) max

let part1 (cucumberMap : CucumberMap) =
    let rightUnlocked, downUnlocked = getInitialUnlocked cucumberMap

    let startingState =
        { Map = cucumberMap
          RightUnlocked = Set rightUnlocked
          DownUnlocked = Set downUnlocked }

    let _, n = doStep startingState 0 500

    n
    // Correct Answer: 498 took: 1915ms

let input = readLinesWithSlashComments "day25.txt" |> parseInput

Helper.measurePart1 part1 input


