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

type SideRoomLetter = | RoomA | RoomB | RoomC | RoomD

type Location =
    | Hallway of HallwayPosition
    | SideRoom of SideRoomLetter * SideRoomPosition

let allHallwayPositions =
    [ FarLeft; ALeft; AB; BC; CD; DRight; FarRight ]

let allHallwayLocations =
    [ Hallway FarLeft; Hallway ALeft; Hallway AB; Hallway BC
      Hallway CD; Hallway DRight; Hallway FarRight ]

let pathStepCost a b =
   match a, b with
   | Hallway FarLeft, Hallway ALeft -> 1
   | Hallway ALeft, Hallway AB -> 2
   | Hallway AB, Hallway BC -> 2
   | Hallway BC, Hallway CD -> 2
   | Hallway CD, Hallway DRight -> 2
   | Hallway DRight, Hallway FarRight -> 1
   | Hallway FarRight, Hallway DRight -> 1
   | Hallway DRight, Hallway CD -> 2
   | Hallway CD, Hallway BC -> 2
   | Hallway BC, Hallway AB -> 2
   | Hallway AB, Hallway ALeft -> 2
   | Hallway ALeft, Hallway FarLeft -> 1
   | Hallway ALeft, SideRoom (RoomA, Top) -> 2
   | Hallway AB, SideRoom (RoomA, Top) -> 2
   | Hallway AB, SideRoom (RoomB, Top) -> 2
   | Hallway BC, SideRoom (RoomB, Top) -> 2
   | Hallway BC, SideRoom (RoomC, Top) -> 2
   | Hallway CD, SideRoom (RoomC, Top) -> 2
   | Hallway CD, SideRoom (RoomD, Top) -> 2
   | Hallway DRight, SideRoom (RoomD, Top) -> 2
   | SideRoom (RoomA, Top), Hallway AB  -> 2
   | SideRoom (RoomB, Top), Hallway AB -> 2
   | SideRoom (RoomB, Top), Hallway BC -> 2
   | SideRoom (RoomC, Top), Hallway BC -> 2
   | SideRoom (RoomC, Top), Hallway CD -> 2
   | SideRoom (RoomD, Top), Hallway CD -> 2
   | SideRoom (RoomD, Top), Hallway DRight -> 2
   | SideRoom (x, Top), SideRoom (y, Bottom) when x = y -> 1
   | SideRoom (x, Bottom), SideRoom (y, Top) when x = y -> 1
   | _, _ -> failwithf "Invalid path step from: (%A) to (%A)" a b

let pathCost startLocation path =
    startLocation::path
    |> List.pairwise
    |> List.map (fun (a, b) -> pathStepCost a b)
    |> List.sum

// Note this creates a path in reverse, and includes the from, but not the
// target location. This doesn't feel right
let rec pathToPosition locationA locationB =
    match locationA, locationB with
    | SideRoom (room, x), Hallway FarLeft ->
        (Hallway ALeft)::(pathToPosition (SideRoom (room, x)) (Hallway ALeft))
    | SideRoom (room, x), Hallway FarRight ->
        (Hallway DRight)::(pathToPosition (SideRoom (room, x)) (Hallway DRight))
    | SideRoom (roomX, x), SideRoom (roomY, Bottom) ->
        SideRoom (RoomB, Top)::(pathToPosition (SideRoom (roomX, x)) (SideRoom (roomX, Top)))
    | SideRoom (room, Bottom), x ->
        (pathToPosition (SideRoom (room, Top))) x @ [SideRoom (room, Bottom)]

    | SideRoom (RoomA, Top), Hallway ALeft -> [SideRoom (RoomA, Top)]
    | SideRoom (RoomA, Top), Hallway AB -> [SideRoom (RoomA, Top)]
    | SideRoom (RoomA, Top), Hallway BC -> [Hallway AB; SideRoom (RoomA, Top)]
    | SideRoom (RoomA, Top), Hallway CD -> [Hallway BC; Hallway AB; SideRoom (RoomA, Top)]
    | SideRoom (RoomA, Top), Hallway DRight -> [Hallway CD; Hallway BC; Hallway AB; SideRoom (RoomA, Top)]
    | SideRoom (RoomA, Top), SideRoom (RoomB, Top) -> [Hallway AB; SideRoom (RoomA, Top)]
    | SideRoom (RoomA, Top), SideRoom (RoomC, Top) -> [Hallway BC; Hallway AB; SideRoom (RoomA, Top)]
    | SideRoom (RoomA, Top), SideRoom (RoomD, Top) -> [Hallway CD; Hallway BC; Hallway AB; SideRoom (RoomA, Top)]

    | SideRoom (RoomB, Top), Hallway ALeft -> [Hallway AB; SideRoom (RoomB, Top)]
    | SideRoom (RoomB, Top), Hallway AB -> [SideRoom (RoomB, Top)]
    | SideRoom (RoomB, Top), Hallway BC -> [SideRoom (RoomB, Top)]
    | SideRoom (RoomB, Top), Hallway CD -> [Hallway BC; SideRoom (RoomB, Top)]
    | SideRoom (RoomB, Top), Hallway DRight -> [Hallway CD; Hallway BC; SideRoom (RoomB, Top)]
    | SideRoom (RoomB, Top), SideRoom (RoomA, x) -> [Hallway AB; SideRoom (RoomB, Top)]
    | SideRoom (RoomB, Top), SideRoom (RoomC, x) -> [Hallway BC; SideRoom (RoomB, Top)]
    | SideRoom (RoomB, Top), SideRoom (RoomD, x) -> [Hallway CD; Hallway BC; SideRoom (RoomB, Top)]

    | SideRoom (RoomC, Top), Hallway ALeft -> [Hallway AB; Hallway BC; SideRoom (RoomC, Top)]
    | SideRoom (RoomC, Top), Hallway AB -> [Hallway BC; SideRoom (RoomC, Top)]
    | SideRoom (RoomC, Top), Hallway BC -> [SideRoom (RoomC, Top)]
    | SideRoom (RoomC, Top), Hallway CD -> [SideRoom (RoomC, Top)]
    | SideRoom (RoomC, Top), Hallway DRight -> [Hallway CD; SideRoom (RoomC, Top)]
    | SideRoom (RoomC, Top), SideRoom (RoomA, x) -> [Hallway AB; Hallway BC; SideRoom (RoomC, Top)]
    | SideRoom (RoomC, Top), SideRoom (RoomB, x) -> [Hallway BC; SideRoom (RoomC, Top)]
    | SideRoom (RoomC, Top), SideRoom (RoomD, x) -> [Hallway CD; SideRoom (RoomC, Top)]

    | SideRoom (RoomD, Top), Hallway ALeft -> [Hallway AB; Hallway BC; Hallway CD; SideRoom (RoomD, Top)]
    | SideRoom (RoomD, Top), Hallway AB -> [Hallway BC; Hallway CD; SideRoom (RoomD, Top)]
    | SideRoom (RoomD, Top), Hallway BC -> [Hallway CD; SideRoom (RoomD, Top)]
    | SideRoom (RoomD, Top), Hallway CD -> [SideRoom (RoomD, Top)]
    | SideRoom (RoomD, Top), Hallway DRight -> [SideRoom (RoomD, Top)]
    | SideRoom (RoomD, Top), SideRoom (RoomA, x) -> [Hallway AB; Hallway BC; Hallway CD; SideRoom (RoomD, Top)]
    | SideRoom (RoomD, Top), SideRoom (RoomB, x) -> [Hallway BC; Hallway CD; SideRoom (RoomD, Top)]
    | SideRoom (RoomD, Top), SideRoom (RoomC, x) -> [Hallway CD; SideRoom (RoomD, Top)]

    // invalid moves
    | Hallway _, Hallway _
    | SideRoom (RoomA, _), SideRoom (RoomA, _)
    | SideRoom (RoomB, _), SideRoom (RoomB, _)
    | SideRoom (RoomC, _), SideRoom (RoomC, _)
    | SideRoom (RoomD, _), SideRoom (RoomD, _) -> failwithf "Invalid Move"

    // hallway to side room is just the inverse of a side room to a hallway
    | Hallway x, y -> pathToPosition y (Hallway x) |> List.rev


let rec stepsToPosition locationA locationB =
    match locationA, locationB with
    | SideRoom (RoomA, Top), Hallway FarLeft -> 3
    | SideRoom (RoomA, Top), Hallway ALeft -> 2
    | SideRoom (RoomA, Top), Hallway AB -> 2
    | SideRoom (RoomA, Top), Hallway BC -> 4
    | SideRoom (RoomA, Top), Hallway CD -> 6
    | SideRoom (RoomA, Top), Hallway DRight -> 8
    | SideRoom (RoomA, Top), Hallway FarRight -> 9
    | SideRoom (RoomA, Top), SideRoom (RoomB, x) -> match x with | Top -> 4 | Bottom -> 5
    | SideRoom (RoomA, Top), SideRoom (RoomC, x) -> match x with | Top -> 6 | Bottom -> 7
    | SideRoom (RoomA, Top), SideRoom (RoomD, x) -> match x with | Top -> 8 | Bottom -> 9
    | SideRoom (RoomA, Bottom), x -> stepsToPosition (SideRoom (RoomA, Top)) x + 1

    | SideRoom (RoomB, Top), Hallway FarLeft -> 5
    | SideRoom (RoomB, Top), Hallway ALeft -> 4
    | SideRoom (RoomB, Top), Hallway AB -> 2
    | SideRoom (RoomB, Top), Hallway BC -> 2
    | SideRoom (RoomB, Top), Hallway CD -> 4
    | SideRoom (RoomB, Top), Hallway DRight -> 6
    | SideRoom (RoomB, Top), Hallway FarRight -> 7
    | SideRoom (RoomB, Top), SideRoom (RoomA, x) -> match x with | Top -> 4 | Bottom -> 5
    | SideRoom (RoomB, Top), SideRoom (RoomC, x) -> match x with | Top -> 4 | Bottom -> 5
    | SideRoom (RoomB, Top), SideRoom (RoomD, x) -> match x with | Top -> 6 | Bottom -> 7
    | SideRoom (RoomB, Bottom), x -> stepsToPosition (SideRoom (RoomB, Top)) x + 1

    | SideRoom (RoomC, Top), Hallway FarLeft -> 7
    | SideRoom (RoomC, Top), Hallway ALeft -> 6
    | SideRoom (RoomC, Top), Hallway AB -> 4
    | SideRoom (RoomC, Top), Hallway BC -> 2
    | SideRoom (RoomC, Top), Hallway CD -> 2
    | SideRoom (RoomC, Top), Hallway DRight -> 4
    | SideRoom (RoomC, Top), Hallway FarRight -> 5
    | SideRoom (RoomC, Top), SideRoom (RoomA, x) -> match x with | Top -> 6 | Bottom -> 7
    | SideRoom (RoomC, Top), SideRoom (RoomB, x) -> match x with | Top -> 4 | Bottom -> 5
    | SideRoom (RoomC, Top), SideRoom (RoomD, x) -> match x with | Top -> 4 | Bottom -> 5
    | SideRoom (RoomC, Bottom), x -> stepsToPosition (SideRoom (RoomC, Top)) x + 1

    | SideRoom (RoomD, Top), Hallway FarLeft -> 9
    | SideRoom (RoomD, Top), Hallway ALeft -> 8
    | SideRoom (RoomD, Top), Hallway AB -> 6
    | SideRoom (RoomD, Top), Hallway BC -> 4
    | SideRoom (RoomD, Top), Hallway CD -> 2
    | SideRoom (RoomD, Top), Hallway DRight -> 2
    | SideRoom (RoomD, Top), Hallway FarRight -> 3
    | SideRoom (RoomD, Top), SideRoom (RoomA, x) -> match x with | Top -> 8 | Bottom -> 9
    | SideRoom (RoomD, Top), SideRoom (RoomB, x) -> match x with | Top -> 6 | Bottom -> 7
    | SideRoom (RoomD, Top), SideRoom (RoomC, x) -> match x with | Top -> 4 | Bottom -> 5
    | SideRoom (RoomD, Bottom), x -> stepsToPosition (SideRoom (RoomD, Top)) x + 1

    // invalid moves
    | Hallway _, Hallway _
    | SideRoom (RoomA, _), SideRoom (RoomA, _)
    | SideRoom (RoomB, _), SideRoom (RoomB, _)
    | SideRoom (RoomC, _), SideRoom (RoomC, _)
    | SideRoom (RoomD, _), SideRoom (RoomD, _) -> failwithf "Invalid Move"

    // hallway to side room is just the inverse of a side room to a hallway
    | Hallway x, y -> stepsToPosition y (Hallway x)

//type SimpleState =
//    { Hallway : string list
//      SideRoomA : string list
//      SideRoomB : string list
//      SideRoomC : string list
//      SideRoomD : string list }

type State =
    { Hallway : (string * HallwayPosition) list
      // The sideroom lists are ordered from top to bottom. If there's only a
      // single item, they are on the bottom
      SideRoomA : string list
      SideRoomB : string list
      SideRoomC : string list
      SideRoomD : string list }

module State =

    let sideRoom room state =
        match room with
        | RoomA -> state.SideRoomA
        | RoomB -> state.SideRoomB
        | RoomC -> state.SideRoomC
        | RoomD -> state.SideRoomD

    let removeFromHallway hallwayPos state =
        { state with
            Hallway = state.Hallway |> List.filter (fun (_, p) -> p <> hallwayPos) }

    let removeNextFromRoom room state =
        let removeFromSideRoom sideRoom =
            match sideRoom with
            | [] -> failwithf "SideRoom %A is empty. Can't remove next." room
            | xs -> List.tail xs
        match room with
        | RoomA -> { state with SideRoomA = removeFromSideRoom state.SideRoomA }
        | RoomB -> { state with SideRoomB = removeFromSideRoom state.SideRoomB }
        | RoomC -> { state with SideRoomC = removeFromSideRoom state.SideRoomC }
        | RoomD -> { state with SideRoomD = removeFromSideRoom state.SideRoomD }

    let addAmphipodToHomeRoom room state =
        let addToRoom amphipod sideRoom =
            match sideRoom with
            | [] -> [amphipod]
            | [a] when a = amphipod -> [amphipod; amphipod]
            | [x] -> failwithf "Can't add to sideroom with non matching amphipod present. Room %A, amphipod: %A " room x
            | _ -> failwithf "Sideroom %A is already full, cannot add any more amphipods" room
        match room with
        | RoomA -> { state with SideRoomA = addToRoom "A" state.SideRoomA }
        | RoomB -> { state with SideRoomB = addToRoom "B" state.SideRoomB }
        | RoomC -> { state with SideRoomC = addToRoom "C" state.SideRoomC }
        | RoomD -> { state with SideRoomD = addToRoom "D" state.SideRoomD }

    let setSideRoom room sideRoomState state =
        match room with
        | RoomA -> { state with SideRoomA = sideRoomState }
        | RoomB -> { state with SideRoomB = sideRoomState }
        | RoomC -> { state with SideRoomC = sideRoomState }
        | RoomD -> { state with SideRoomD = sideRoomState }

    let addToSideRoomAt amphipod room roomPos state =
        let updateSideRoom amphipod roomPos sideRoom =
            match sideRoom, roomPos with
            | [], Bottom -> [amphipod]
            | [], Top -> failwithf "Can't add to top of sideroom when the room is empty. Room %A" room
            | [a], Top when a = amphipod -> [amphipod; amphipod]
            | [x], Top -> failwithf "Can't add to sideroom with non matching amphipod present. Room %A, amphipod: %A " room x
            | [x], Bottom -> failwithf "Can't add to bottom of sideroom when it already has an amphipod. Room %A" room
            | _ -> failwithf "Sideroom %A is already full, cannot add any more amphipods" room
        let sideRoom = sideRoom room state
        setSideRoom room (updateSideRoom amphipod roomPos sideRoom) state

    let addToHallway amphipod hallwayPos state =
        match state.Hallway |> List.tryFind (fun (_,p) -> p = hallwayPos) with
        | Some x -> failwithf "An amphipod: %A already exists at hallway pos %A" x hallwayPos
        | None -> { state with Hallway = (amphipod, hallwayPos)::state.Hallway }

    let removeFromState amphipod location state =
        match location with
        | Hallway hallwayPos -> removeFromHallway amphipod hallwayPos state
        | SideRoom (room, roomPos) -> removeNextFromRoom amphipod room roomPos state

    let addToState amphipod location state =
        match location with
        | Hallway hallwayPos -> addToHallway amphipod hallwayPos state
        | SideRoom (room, roomPos) -> addToSideRoomAt amphipod room roomPos state


let goalState =
    { Hallway = []
      SideRoomA = ["A"; "A"]
      SideRoomB = ["B"; "B"]
      SideRoomC = ["C"; "C"]
      SideRoomD = ["D"; "D"] }

let desiredSideRoom state amphipod =
    match amphipod with
    | "A" -> state.SideRoomA
    | "B" -> state.SideRoomB
    | "C" -> state.SideRoomC
    | "D" -> state.SideRoomD
    | _ -> failwithf "Invalid amphipod type: %s" amphipod

let amphipodSideRoom amphipod =
    match amphipod with
    | "A" -> RoomA
    | "B" -> RoomB
    | "C" -> RoomC
    | "D" -> RoomD
    | _ -> failwithf "Invalid amphipod type: %s" amphipod

let roomAmphipod room =
    match room with
    | RoomA -> "A"
    | RoomB -> "B"
    | RoomC -> "C"
    | RoomD -> "D"

let hallwayPathToRoom amphipod pos =
    let rec hallwayPathToRoom' amphipod pos path =
        match pos with
        | FarLeft ->
            hallwayPathToRoom' amphipod ALeft (ALeft::path)
        | ALeft ->
            match amphipod with
            | "A" -> path
            | _ -> hallwayPathToRoom' amphipod AB (AB::path)
        | AB ->
            match amphipod with
            | "A" | "B" -> path
            | _ -> hallwayPathToRoom' amphipod BC (BC::path)
        | BC ->
            match amphipod with
            | "A" -> hallwayPathToRoom' amphipod AB (AB::path)
            | "C" | "B" -> path
            | "D" -> hallwayPathToRoom' amphipod CD (CD::path)
            | _ -> failwith "Invalid amphipod"
        | CD ->
            match amphipod with
            | "C" | "D" -> path
            | _ -> hallwayPathToRoom' amphipod BC (BC::path)
        | DRight ->
            match amphipod with
            | "D" -> path
            | _ -> hallwayPathToRoom' amphipod CD (CD::path)
        | FarRight ->
            hallwayPathToRoom' amphipod DRight (DRight::path)
    hallwayPathToRoom' amphipod pos []

let amphipodAtLocation state location =
    match location with
    | Hallway h ->
        state.Hallway
        |> List.tryFind (fun (_, pos) -> pos = h)
        |> Option.map fst
    | SideRoom (room, Top) ->
        match State.sideRoom room state with
        | [top; _] -> Some top
        | _ -> None
    | SideRoom (room, Bottom) ->
        match State.sideRoom room state with
        | [_; bottom] -> Some bottom
        | [bottom] -> Some bottom
        | _ -> None

let isMoveFree state from target =
    pathToPosition from target
    |> List.forall (fun step ->
        amphipodAtLocation state step
        |> Option.isNone)

type Move = { From : Location; To: Location }

let possibleHallwayToRoomMove state (amphipod, pos : HallwayPosition) =
    // get the sideroom this amphipod needs to be in. That's the only possible
    // room they could move to
    Some (desiredSideRoom state amphipod)
    // items in the hallway can only move into their sideroom if
    // it's empty or it contains their own type of amphipod
    |> Option.filter (fun room -> List.isEmpty room || List.head room = amphipod)
    // items in the hallway can't move through other items in the hallway
    |> Option.map (fun _ ->
        { From = Hallway pos; To = SideRoom (amphipodSideRoom amphipod, Top) })
    |> Option.filter (fun move ->
        amphipodSideRoom amphipod
        |> fun roomType -> isMoveFree state move.From move.To)
    // If we're still able to do the move, return the next state
    |> Option.map (fun _ ->
        state
        |> State.removeFromHallway pos
        |> State.addAmphipodToHomeRoom (amphipodSideRoom amphipod))

let possibleHallwayToRoomMoves state =
    state.Hallway |> List.choose (possibleHallwayToRoomMove state)

let possibleSideRoomAMove state =
    match state.SideRoomA with
    | [] -> [] // empty room, so no moves to make
    | ["A"]
    | ["A"; "A"] -> [] // already where it should be
    | ["A"; _] -> // A on top, something else underneath
        // need to move the A out to the hallway, so the other item can move out
        // and we can put the A's back in
        allHallwayPositions
        |> List.filter (fun pos -> isMoveFree state (SideRoom (RoomA, Top)) (Hallway pos))
        |> List.map (fun hallwayPos ->
            state
            |> State.removeNextFromRoom RoomA
            |> State.addToHallway "A" hallwayPos)

    | otherAmphipod :: _ -> // another otherAmphipod, with potentially something underneath
        let roomPos = if List.length state.SideRoomA = 1 then Bottom else Top
        let desiredRoom = desiredSideRoom state otherAmphipod
        let desiredRoomType = amphipodSideRoom otherAmphipod
        // get valid position in the side room
        let moveTo =
            // if the desired room is empty, then we want to move to the bottom of that room
            if List.isEmpty desiredRoom then Some (SideRoom (desiredRoomType, Bottom))
            // if the desired room has a single amphipod of the same type then we want to move to the top of that room
            elif desiredRoom = [otherAmphipod] then Some (SideRoom (desiredRoomType, Top))
            // we can always move to a hallway
            else None

        let moveTo = moveTo |> Option.filter (isMoveFree state (SideRoom (RoomA, roomPos)))

        match moveTo with
        | Some location ->
            state
            |> State.removeNextFromRoom RoomA
            |> State.addToState otherAmphipod location
            |> List.singleton
        | None ->
            // We couldn't move the amphipod directly to the room it belonged
            // for whatever reason, so check where we can move it to in the hallway.
            // (Note: The hallway moves are always valid options, but I don't
            // think it makes any sense to consider them if a direct move would
            // have worked)
            allHallwayPositions
            |> List.filter (fun pos -> isMoveFree state (SideRoom (RoomA, roomPos)) (Hallway pos))
            |> List.map (fun hallwayPos ->
                state
                |> State.removeNextFromRoom RoomA
                |> State.addToHallway otherAmphipod hallwayPos)

let possibleSideRoomMoves room state =
    let sideRoom = State.sideRoom room state
    let roomAmphipod = roomAmphipod room
    match sideRoom with
    | [] -> [] // empty room, so no moves to make
    | [x] when x = roomAmphipod -> [] // already where it should be
    | [x; y] when x = y && x = roomAmphipod -> [] // already where it should be
    | [x; _] when x = roomAmphipod -> // room amphipod on top, something else underneath
        // need to move the A out to the hallway, so the other item can move out
        // and we can put the A's back in
        allHallwayPositions
        |> List.filter (fun pos -> isMoveFree state (SideRoom (RoomA, Top)) (Hallway pos))
        |> List.map (fun hallwayPos ->
            state
            |> State.removeNextFromRoom RoomA
            |> State.addToHallway "A" hallwayPos)

    | otherAmphipod :: _ -> // another otherAmphipod, with potentially something underneath
        let roomPos = if List.length sideRoom = 1 then Bottom else Top
        let desiredRoom = desiredSideRoom state otherAmphipod
        let desiredRoomType = amphipodSideRoom otherAmphipod
        // get valid position in the side room
        let moveTo =
            // if the desired room is empty, then we want to move to the bottom of that room
            if List.isEmpty desiredRoom then Some (SideRoom (desiredRoomType, Bottom))
            // if the desired room has a single amphipod of the same type then we want to move to the top of that room
            elif desiredRoom = [otherAmphipod] then Some (SideRoom (desiredRoomType, Top))
            // we can't move into that room, but we can always move to a hallway
            else None

        let moveTo = moveTo |> Option.filter (isMoveFree state (SideRoom (room, roomPos)))

        match moveTo with
        | Some location ->
            state
            |> State.removeNextFromRoom room
            |> State.addToState otherAmphipod location
            |> List.singleton
        | None ->
            // We couldn't move the amphipod directly to the room it belonged
            // for whatever reason, so check where we can move it to in the hallway.
            // (Note: The hallway moves are always valid options, but I don't
            // think it makes any sense to consider them if a direct move would
            // have worked)
            allHallwayPositions
            |> List.filter (fun pos -> isMoveFree state (SideRoom (room, roomPos)) (Hallway pos))
            |> List.map (fun hallwayPos ->
                state
                |> State.removeNextFromRoom room
                |> State.addToHallway otherAmphipod hallwayPos)



let allPossibleSideRoomMoves state =
    [ RoomA; RoomB; RoomC; RoomD ]
    |> List.collect (fun r -> possibleSideRoomMoves r state)

let neighbours state =
    // The neighbour states are all the possible moves
    // All items in the hallway could move to a valid side room
    possibleHallwayToRoomMoves state
    // The top items in each side room could move out into any of the
    // hallway positions, or into a valid side room
    @ allPossibleSideRoomMoves state

let parseInput lines =
    // just get the 2 lines with data we need
    let [line1; line2] = lines |> Seq.skip 2 |> Seq.take 2 |> Seq.toList

    // extract the amphipod type in each room
    let sideRoomsLine1 =
        String.capture "([ABCD]).*([ABCD]).*([ABCD]).*([ABCD])" line1
    let sideRoomsLine2 =
        String.capture "([ABCD]).*([ABCD]).*([ABCD]).*([ABCD])" line2

    { Hallway = []
      SideRoomA = [sideRoomsLine1.[0]; sideRoomsLine2.[0]]
      SideRoomB = [sideRoomsLine1.[1]; sideRoomsLine2.[1]]
      SideRoomC = [sideRoomsLine1.[2]; sideRoomsLine2.[2]]
      SideRoomD = [sideRoomsLine1.[3]; sideRoomsLine2.[3]] }

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
        let neighbours = neighbours current
        for neighbor in neighbours do
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



