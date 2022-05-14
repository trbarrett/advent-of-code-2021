#load "./Helper.fsx"
open Helper

// Day 23: Amphipod

// (A)mber amphipods require 1 energy per step,
// (B)ronze amphipods require 10 energy,
// (C)opper amphipods require 100, and
// (D)esert ones require 1000

type HallwayPosition =
    | FarLeft | ALeft | AB | BC | CD | DRight | FarRight


type SideRoomPosition = | Top | Bottom

type Location =
    | Hallway of HallwayPosition
    | SideRoomA of SideRoomPosition
    | SideRoomB of SideRoomPosition
    | SideRoomC of SideRoomPosition
    | SideRoomD of SideRoomPosition

let rec stepsToPosition locationA locationB =
    match locationA, locationB with
    | SideRoomA Top, Hallway FarLeft -> 3
    | SideRoomA Top, Hallway ALeft -> 2
    | SideRoomA Top, Hallway AB -> 2
    | SideRoomA Top, Hallway BC -> 4
    | SideRoomA Top, Hallway CD -> 6
    | SideRoomA Top, Hallway DRight -> 8
    | SideRoomA Top, Hallway FarRight -> 9
    | SideRoomA Top, SideRoomB x -> match x with | Top -> 4 | Bottom -> 5
    | SideRoomA Top, SideRoomC x -> match x with | Top -> 6 | Bottom -> 7
    | SideRoomA Top, SideRoomD x -> match x with | Top -> 8 | Bottom -> 9
    | SideRoomA Bottom, x -> stepsToPosition (SideRoomA Top) x + 1

    | SideRoomB Top, Hallway FarLeft -> 5
    | SideRoomB Top, Hallway ALeft -> 4
    | SideRoomB Top, Hallway AB -> 2
    | SideRoomB Top, Hallway BC -> 2
    | SideRoomB Top, Hallway CD -> 4
    | SideRoomB Top, Hallway DRight -> 6
    | SideRoomB Top, Hallway FarRight -> 7
    | SideRoomB Top, SideRoomA x -> match x with | Top -> 4 | Bottom -> 5
    | SideRoomB Top, SideRoomC x -> match x with | Top -> 4 | Bottom -> 5
    | SideRoomB Top, SideRoomD x -> match x with | Top -> 6 | Bottom -> 7
    | SideRoomB Bottom, x -> stepsToPosition (SideRoomB Top) x + 1

    | SideRoomC Top, Hallway FarLeft -> 7
    | SideRoomC Top, Hallway ALeft -> 6
    | SideRoomC Top, Hallway AB -> 4
    | SideRoomC Top, Hallway BC -> 2
    | SideRoomC Top, Hallway CD -> 2
    | SideRoomC Top, Hallway DRight -> 4
    | SideRoomC Top, Hallway FarRight -> 5
    | SideRoomC Top, SideRoomA x -> match x with | Top -> 6 | Bottom -> 7
    | SideRoomC Top, SideRoomB x -> match x with | Top -> 4 | Bottom -> 5
    | SideRoomC Top, SideRoomD x -> match x with | Top -> 4 | Bottom -> 5
    | SideRoomC Bottom, x -> stepsToPosition (SideRoomC Top) x + 1

    | SideRoomD Top, Hallway FarLeft -> 9
    | SideRoomD Top, Hallway ALeft -> 8
    | SideRoomD Top, Hallway AB -> 6
    | SideRoomD Top, Hallway BC -> 4
    | SideRoomD Top, Hallway CD -> 2
    | SideRoomD Top, Hallway DRight -> 2
    | SideRoomD Top, Hallway FarRight -> 3
    | SideRoomD Top, SideRoomA x -> match x with | Top -> 8 | Bottom -> 9
    | SideRoomD Top, SideRoomB x -> match x with | Top -> 6 | Bottom -> 7
    | SideRoomD Top, SideRoomC x -> match x with | Top -> 4 | Bottom -> 5
    | SideRoomD Bottom, x -> stepsToPosition (SideRoomD Top) x + 1

    // invalid moves
    | Hallway _, Hallway _
    | SideRoomA _, SideRoomA _
    | SideRoomB _, SideRoomB _
    | SideRoomC _, SideRoomC _
    | SideRoomD _, SideRoomD _ -> failwithf "Invalid Move"

    // hallway to side room is just the inverse of a side room to a hallway
    | Hallway x, y -> stepsToPosition y (Hallway x)

//type SimpleState =
//    { Hallway : string list
//      SideRoomA : string list
//      SideRoomB : string list
//      SideRoomC : string list
//      SideRoomD : string list }

type SideRooms =
    { SideRoomA : string list
      SideRoomB : string list
      SideRoomC : string list
      SideRoomD : string list }

type State =
    { Hallway : (string * HallwayPosition) list
      SideRooms : SideRooms }

let goalState =
    { Hallway = []
      SideRooms =
        { SideRoomA = ["A"; "A"]
          SideRoomB = ["B"; "B"]
          SideRoomC = ["C"; "C"]
          SideRoomD = ["D"; "D"] } }

let desiredSideRoom state amphipod =
    match amphipod with
    | "A" -> state.SideRooms.SideRoomA
    | "B" -> state.SideRooms.SideRoomB
    | "C" -> state.SideRooms.SideRoomC
    | "D" -> state.SideRooms.SideRoomD
    | _ -> failwithf "Invalid amphipod type: %s" amphipod

let hallwayPathToRoom amphipod pos =
    let rec hallwayPathToRoom' amphipod pos path =
        match pos with
        | FarLeft ->
            hallwayPathToRoom' amphipod ALeft (ALeft::path)
        | ALeft ->
            match amphipod with
            | "a" -> path
            | _ -> hallwayPathToRoom' amphipod AB (AB::path)
        | AB ->
            match amphipod with
            | "a" | "b" -> path
            | _ -> hallwayPathToRoom' amphipod BC (BC::path)
        | BC ->
            match amphipod with
            | "a" -> hallwayPathToRoom' amphipod AB (AB::path)
            | "c" | "b" -> path
            | "d" -> hallwayPathToRoom' amphipod CD (CD::path)
            | _ -> failwith "Invalid amphipod"
        | CD ->
            match amphipod with
            | "c" | "d" -> path
            | _ -> hallwayPathToRoom' amphipod BC (BC::path)
        | DRight ->
            match amphipod with
            | "d" -> path
            | _ -> hallwayPathToRoom' amphipod CD (CD::path)
        | FarRight ->
            hallwayPathToRoom' amphipod DRight (DRight::path)
    hallwayPathToRoom' amphipod pos []

let isMoveFree state amphipod from target =
    match from, target with
    | Hallway h, Hallway t -> failwith "invalid to move between hallway positions"
    | Hallway h, SideRoom s ->
        hallwayPathToRoom amphipod h
        |> List.forall (fun step ->
            state.Hallway
            |> List.tryFind (fun (_, pos) -> pos = step)
            |> Option.isNone)
    | s, Hallway h ->

let possibleHallwayToRoomMove state (amphipod, (pos : HallwayPosition)) =
    // get the sideroom this amphipod needs to be in. That's the only possible
    // room they could move to
    Some (desiredSideRoom state amphipod)
    // items in the hallway can only move into their sideroom if
    // it's empty or it contains their own type of amphipod
    |> Option.filter (fun room -> List.isEmpty room || List.head room = amphipod)
    // items in the hallway can't move through other items in the hallway
    |> Option.filter (fun _ ->
        hallwayPathToRoom amphipod pos
        |> List.forall (fun step ->
            state.Hallway
            |> List.tryFind (fun (_, pos) -> pos = step)
            |> Option.isNone))
    // If we're still able to do the move, return the next state
    |> Option.map (fun _ ->
        { state with
            Hallway = state.Hallway |> List.filter (fun (_, p) -> p <> pos)
            SideRooms =
                { state.SideRooms with
                    SideRoomA = if amphipod = "a"
                                then amphipod::state.SideRooms.SideRoomA
                                else state.SideRooms.SideRoomA
                    SideRoomB = if amphipod = "b"
                                then amphipod::state.SideRooms.SideRoomB
                                else state.SideRooms.SideRoomB
                    SideRoomC = if amphipod = "c"
                                then amphipod::state.SideRooms.SideRoomC
                                else state.SideRooms.SideRoomC
                    SideRoomD = if amphipod = "d"
                                then amphipod::state.SideRooms.SideRoomD
                                else state.SideRooms.SideRoomD }})

let possibleHallwayToRoomMoves state =
    state.Hallway |> List.choose (possibleHallwayToRoomMove state)

//let possibleSideRoomAMove state =
//    match state.SideRoomA with
//    | ["A"]
//    | ["A"; "A"] -> [] // already where it should be
//    | ["A"; _] ->
//        // need to move the A out to the hallway, so the other item can move in
//
//    | other::_ ->
//        // can move other to an empty matching side room, or if that's not available
//        // can move other to the hallway
//
//
//
//let possibleSideRoomMoves state =
//    state.SideRoomA

let neighbours state =
    // The neighbour states are all the possible moves
    [
        // All items in the hallway could move to a valid side room
        possibleHallwayToRoomMoves state
        // The top items in each side room could move out into any of the
        // hallway positions, or into a valid side room
        possibleSideRoomMoves state ]
    |> List.collect id


let parseInput lines =
    // just get the 2 lines with data we need
    let [line1; line2] = lines |> Seq.skip 2 |> Seq.take 2 |> Seq.toList

    // extract the amphipod type in each room
    let sideRoomsLine1 =
        String.capture "([ABCD]).*([ABCD]).*([ABCD]).*([ABCD])" line1
    let sideRoomsLine2 =
        String.capture "([ABCD]).*([ABCD]).*([ABCD]).*([ABCD])" line2

    { Hallway = []
      SideRooms =
        { SideRoomA = [sideRoomsLine1.[0]; sideRoomsLine2.[0]]
          SideRoomB = [sideRoomsLine1.[1]; sideRoomsLine2.[1]]
          SideRoomC = [sideRoomsLine1.[2]; sideRoomsLine2.[2]]
          SideRoomD = [sideRoomsLine1.[3]; sideRoomsLine2.[3]] } }

let input = readLinesWithSlashComments "day23-example1.txt" |> parseInput

//let reconstruct_path cameFrom current =
//    total_path := {current}
//    while current in cameFrom.Keys:
//        current := cameFrom[current]
//        total_path.prepend(current)
//    return total_path

// A* finds a path from start to goal.
// h is the heuristic function. h(n) estimates the cost to reach goal from node n.
let astar start goal h =
    // The set of discovered nodes that may need to be (re-)expanded.
    // Initially, only the start node is known.
    // This is usually implemented as a min-heap or priority queue rather than a hash-set.
    let openSet = Set.singleton start

    // For node n, cameFrom[n] is the node immediately preceding it on the cheapest path from start
    // to n currently known.
    let cameFrom = Map

    // For node n, gScore[n] is the cost of the cheapest path from start to n currently known.
    // gScore is map with default value of Infinity
    let gScore = Map [start, Some 0]

    // For node n, fScore[n] := gScore[n] + h(n). fScore[n] represents our current best guess as to
    // how short a path from start to finish can be if it goes through n.
    //fScore := map with default value of Infinity
    //fScore[start] := h(start)
    let fScore = Set ((h start), start)


    //while openSet is not empty
    let folder openSet fScore =
        match Set.isEmpty openSet with
        | true -> failure
        | false ->
            // This operation can occur in O(Log(N)) time if openSet is a min-heap or a priority queue
            //current := the node in openSet having the lowest fScore[] value
            let current = fScore |> Set.minElement
            if current = goalState
                then reconstruct_path(cameFrom, current)
            else
                let openSet = openSet |> Map.remove current

        openSet.Remove(current)
        for each neighbor of current
            // d(current,neighbor) is the weight of the edge from current to neighbor
            // tentative_gScore is the distance from start to the neighbor through current
            tentative_gScore := gScore[current] + d(current, neighbor)
            if tentative_gScore < gScore[neighbor]
                // This path to neighbor is better than any previous one. Record it!
                cameFrom[neighbor] := current
                gScore[neighbor] := tentative_gScore
                fScore[neighbor] := tentative_gScore + h(neighbor)
                if neighbor not in openSet
                    openSet.add(neighbor)

    // Open set is empty but goal was never reached
    return failure
//let part1 (simpleState : SimpleState) =
//    countOnCubes exclusiveSteps
    // Correct Answer:  took:

// Solution space feels large when we consider every possible step.
// Perhaps we can simplify it by ignoring the steps for the first part.
// We can just think about in the hallway or not, and come up with all the possible solutions.

// We have 4 stacks, each with a size of 2
// And we have a bag of amphipods in the hallway

// From there we follow the other rules:
// (A) Can't move into a room that's not for them
// (B) Can't move into a room if it has a amphipod that doesn't match

// Obvious solution is to move everything out, then everything in. (except for
// those that are already in). And we can do that, but for one less amphipod
// each time.

// I think a good hueristic would to have the least amount of amphipods in the hallway
// at any one time.
//
// We can start by looking for the easiest side room to fill.
// (1) The easiest is when a room is empty and so we can fill it immediately.
//     If it is empty we will fill it immediatley with any amphipod in the
//     hallway, followed by any available to exit a side room. If one is on the
//     bottom of a side room we'll move the amphipod in the way out, then
//     the correct one in.
// (2) Next is when the bottom amphipod already matches the room its in. Then
//     we just need to move the top one out, and the correct one in. Tie breaker
//     in that case would be where the other one we need is in the hallway or
//     at the top of another side room.
// (3) The next easiest would be where 1 matches the room it's in at the top.
//     In that case we'd need to move the top one out, then the bottom one out,
//     and the top one back in.
//     Tie breaker then again is where the other to fill is in the hallway or at
//     the top of another side room.
// (4) If we don't have any of those cases, then none of the side rooms has
//     any matching amphipods. I think this can be further subdivided into 2
//     cases:
//       (4a) If there's only one amphipod in a side room, moving it out creates
//            space for others to fill.
//       (4b) If both amphipods in a side room are the same, we know we'll be
//            able to put them into a the right room if we clear it. So that seems
//            like a good next step. Tie breaker if we can put them into a room
//            that also has two of the same.
//       (4c) If the two amphipods in a side room are not the same this is the
//            hardest case. We'll empty them both and then we'll be in case (1)
//            - an empty room that's easy to fill. So the tiebreaker here would
//            be how easy it is to fill this room with the correct amphipods.
//            The more in the hallway or
//
// Now I'm not sure if these heuristics are correct. They are just intuitive
// guesses. I'll try out the code and see if it matches example 1.
//
// This doesn't talk into account costs. We could use that for further tie
// breakers perhaps. Probably best just to let the algo do it's thing and
// try multiple cases at the same time... With D being so much more expensive
// we could do quite a bit of flip/floping around with A and B before touching
// D. I'm not sure if that will mess up our heuristics at all.
//
// We could of course not use a heuristic approach. We could just search the
// state space using djikstra's graph search algorithm. My feeling is that's
// going to have a crazy large state space with A's and B's just flapping around
// a whole lot. But maybe not. And the implementation for that shouldn't be too
// hard.
// Another option is to use the A* algorithm, with us giving a score to each of
// our heuristics above. It's hard to give an admissible score to our heuristcs
// though.
// But we can give a pretty easy heuristic for a normal node, which is the cost
// of the node * the amount of steps to the side room it needs to be in. The
// heuristic needs to be to meet the end goal though, so that's going to be a
// sum of all the calculated distances for all the amphipods. That could work
// though.
//
// So now I have 2 good options.
// - My logical deduction option + some djistra search after
// - A* algorithm with energy cost hueristic
// The advantage of the A* algorithm is in the dumbness of the implementation,
// and being able to use the real board state from the get go. It still feels
// like it would take too long... but computers are incredibly fast...
//
// NOTE: an amphipods can never stop outside of a side room, so there's only ever
// 7 spots to consider in the hallway: FarLeft, ALeft, AB, BC, CD, DRight, FarRight
Helper.measurePart1 part1 input



