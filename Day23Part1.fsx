#load "./Helper.fsx"
open Helper
open System

// Day 23: Amphipod

// This day is about moving items between rooms and a hallway in the most
// efficient way possible, in a similar manner to the towers of hanoi problem.
//
// The approach was to use the A-star algorithm to find the best way through
// a large state tree. I took the standard algorithm and modified it to be
// a pure immutable functional style, which slowed down performance quite a bit.
// There's a Mutable version of this with a mutable A-start algorithm, but it
// doesn't improve the speed enough.
//
// I have a feeling the A* approach isn't best approach. The difference between
// the cost of A (1), B (10) and the cost of D (1000) means that we'll waste
// time trying lots of inefficient variations of A and B before doing required
// moves with D. Perhaps some type of constraint propagation version would be a
// better approach.
//
// I'm not too happy with the model I used for the state. Keeping a logical map
// of a bunch of landmarks proved to be overly complex to use in practice. A
// simpler 2d representation of the layout may have proved simpler to use. It
// probably would have required a lot less work to support the changes for part
// 2.


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

// (A)mber amphipods require 1 energy per step,
// (B)ronze amphipods require 10 energy,
// (C)opper amphipods require 100, and
// (D)esert ones require 1000
let amphipodCost = function
    | "A" -> 1.
    | "B" -> 10.
    | "C" -> 100.
    | "D" -> 1000.

let rec pathToPosition' (locationA, locationB) =
    match locationA, locationB with
    // fix these ones
    | SideRoom (room, x), Hallway FarLeft ->
        (pathToPosition' (SideRoom (room, x), Hallway ALeft)) @ [Hallway FarLeft]
    | SideRoom (room, x), Hallway FarRight ->
        (pathToPosition' (SideRoom (room, x), Hallway DRight)) @ [Hallway FarRight]
    | SideRoom (roomX, x), SideRoom (roomY, Bottom) ->
        (pathToPosition' (SideRoom (roomX, x), SideRoom (roomY, Top))) @ [SideRoom (roomY, Bottom)]
    | SideRoom (room, Bottom), x ->
        SideRoom (room, Top)::(pathToPosition' (SideRoom (room, Top), x))

    | SideRoom (RoomA, Top), Hallway ALeft -> [Hallway ALeft]
    | SideRoom (RoomA, Top), Hallway AB -> [Hallway AB]
    | SideRoom (RoomA, Top), Hallway BC -> [Hallway AB; Hallway BC]
    | SideRoom (RoomA, Top), Hallway CD -> [Hallway AB; Hallway BC; Hallway CD]
    | SideRoom (RoomA, Top), Hallway DRight -> [Hallway AB; Hallway BC; Hallway CD; Hallway DRight]
    | SideRoom (RoomA, Top), SideRoom (RoomB, Top) -> [Hallway AB; SideRoom (RoomB, Top)]
    | SideRoom (RoomA, Top), SideRoom (RoomC, Top) -> [Hallway AB; Hallway BC; SideRoom (RoomC, Top)]
    | SideRoom (RoomA, Top), SideRoom (RoomD, Top) -> [Hallway AB; Hallway BC; Hallway CD; SideRoom (RoomD, Top)]

    | SideRoom (RoomB, Top), Hallway ALeft -> [Hallway AB; Hallway ALeft]
    | SideRoom (RoomB, Top), Hallway AB -> [Hallway AB]
    | SideRoom (RoomB, Top), Hallway BC -> [Hallway BC]
    | SideRoom (RoomB, Top), Hallway CD -> [Hallway BC; Hallway CD]
    | SideRoom (RoomB, Top), Hallway DRight -> [Hallway BC; Hallway CD; Hallway DRight]
    | SideRoom (RoomB, Top), SideRoom (RoomA, Top) -> [Hallway AB; SideRoom (RoomA, Top)]
    | SideRoom (RoomB, Top), SideRoom (RoomC, Top) -> [Hallway BC; SideRoom (RoomC, Top)]
    | SideRoom (RoomB, Top), SideRoom (RoomD, Top) -> [Hallway BC; Hallway CD; SideRoom (RoomD, Top)]

    | SideRoom (RoomC, Top), Hallway ALeft -> [Hallway BC; Hallway AB; Hallway ALeft]
    | SideRoom (RoomC, Top), Hallway AB -> [Hallway BC; Hallway AB]
    | SideRoom (RoomC, Top), Hallway BC -> [Hallway BC]
    | SideRoom (RoomC, Top), Hallway CD -> [Hallway CD]
    | SideRoom (RoomC, Top), Hallway DRight -> [Hallway CD; Hallway DRight]
    | SideRoom (RoomC, Top), SideRoom (RoomA, Top) -> [Hallway BC; Hallway AB; SideRoom (RoomA, Top)]
    | SideRoom (RoomC, Top), SideRoom (RoomB, Top) -> [Hallway BC; SideRoom (RoomB, Top)]
    | SideRoom (RoomC, Top), SideRoom (RoomD, Top) -> [Hallway CD; SideRoom (RoomD, Top)]

    | SideRoom (RoomD, Top), Hallway ALeft -> [Hallway CD; Hallway BC; Hallway AB; Hallway ALeft]
    | SideRoom (RoomD, Top), Hallway AB -> [Hallway CD; Hallway BC; Hallway AB]
    | SideRoom (RoomD, Top), Hallway BC -> [Hallway CD; Hallway BC]
    | SideRoom (RoomD, Top), Hallway CD -> [Hallway CD]
    | SideRoom (RoomD, Top), Hallway DRight -> [Hallway DRight]
    | SideRoom (RoomD, Top), SideRoom (RoomA, Top) -> [Hallway CD; Hallway BC; Hallway AB; SideRoom (RoomA, Top)]
    | SideRoom (RoomD, Top), SideRoom (RoomB, Top) -> [Hallway CD; Hallway BC; SideRoom (RoomB, Top)]
    | SideRoom (RoomD, Top), SideRoom (RoomC, Top) -> [Hallway CD; SideRoom (RoomC, Top)]

    // invalid moves
    | Hallway _, Hallway _
    | SideRoom (RoomA, _), SideRoom (RoomA, _)
    | SideRoom (RoomB, _), SideRoom (RoomB, _)
    | SideRoom (RoomC, _), SideRoom (RoomC, _)
    | SideRoom (RoomD, _), SideRoom (RoomD, _) -> failwithf "Invalid Move %A -> %A" locationA locationB

    // hallway to side room is just the inverse of a side room to a hallway
    | Hallway x, y ->
        pathToPosition' (y,Hallway x)
        |> List.rev
        |> List.tail // because we don't want the starting location
        |> fun lst -> lst @ [y]

let pathToPosition = memoize pathToPosition'

let rec costAToB (locationA, locationB) =
    match locationA, locationB with
    | SideRoom (RoomA, Top), Hallway FarLeft -> 3.
    | SideRoom (RoomA, Top), Hallway ALeft -> 2.
    | SideRoom (RoomA, Top), Hallway AB -> 2.
    | SideRoom (RoomA, Top), Hallway BC -> 4.
    | SideRoom (RoomA, Top), Hallway CD -> 6.
    | SideRoom (RoomA, Top), Hallway DRight -> 8.
    | SideRoom (RoomA, Top), Hallway FarRight -> 9.
    | SideRoom (RoomA, Top), SideRoom (RoomB, x) -> match x with | Top -> 4. | Bottom -> 5.
    | SideRoom (RoomA, Top), SideRoom (RoomC, x) -> match x with | Top -> 6. | Bottom -> 7.
    | SideRoom (RoomA, Top), SideRoom (RoomD, x) -> match x with | Top -> 8. | Bottom -> 9.
    | SideRoom (RoomA, Bottom), x -> costAToB (SideRoom (RoomA, Top), x) + 1.

    | SideRoom (RoomB, Top), Hallway FarLeft -> 5.
    | SideRoom (RoomB, Top), Hallway ALeft -> 4.
    | SideRoom (RoomB, Top), Hallway AB -> 2.
    | SideRoom (RoomB, Top), Hallway BC -> 2.
    | SideRoom (RoomB, Top), Hallway CD -> 4.
    | SideRoom (RoomB, Top), Hallway DRight -> 6.
    | SideRoom (RoomB, Top), Hallway FarRight -> 7.
    | SideRoom (RoomB, Top), SideRoom (RoomA, x) -> match x with | Top -> 4. | Bottom -> 5.
    | SideRoom (RoomB, Top), SideRoom (RoomC, x) -> match x with | Top -> 4. | Bottom -> 5.
    | SideRoom (RoomB, Top), SideRoom (RoomD, x) -> match x with | Top -> 6. | Bottom -> 7.
    | SideRoom (RoomB, Bottom), x -> costAToB (SideRoom (RoomB, Top), x) + 1.

    | SideRoom (RoomC, Top), Hallway FarLeft -> 7.
    | SideRoom (RoomC, Top), Hallway ALeft -> 6.
    | SideRoom (RoomC, Top), Hallway AB -> 4.
    | SideRoom (RoomC, Top), Hallway BC -> 2.
    | SideRoom (RoomC, Top), Hallway CD -> 2.
    | SideRoom (RoomC, Top), Hallway DRight -> 4.
    | SideRoom (RoomC, Top), Hallway FarRight -> 5.
    | SideRoom (RoomC, Top), SideRoom (RoomA, x) -> match x with | Top -> 6. | Bottom -> 7.
    | SideRoom (RoomC, Top), SideRoom (RoomB, x) -> match x with | Top -> 4. | Bottom -> 5.
    | SideRoom (RoomC, Top), SideRoom (RoomD, x) -> match x with | Top -> 4. | Bottom -> 5.
    | SideRoom (RoomC, Bottom), x -> costAToB (SideRoom (RoomC, Top), x) + 1.

    | SideRoom (RoomD, Top), Hallway FarLeft -> 9.
    | SideRoom (RoomD, Top), Hallway ALeft -> 8.
    | SideRoom (RoomD, Top), Hallway AB -> 6.
    | SideRoom (RoomD, Top), Hallway BC -> 4.
    | SideRoom (RoomD, Top), Hallway CD -> 2.
    | SideRoom (RoomD, Top), Hallway DRight -> 2.
    | SideRoom (RoomD, Top), Hallway FarRight -> 3.
    | SideRoom (RoomD, Top), SideRoom (RoomA, x) -> match x with | Top -> 8. | Bottom -> 9.
    | SideRoom (RoomD, Top), SideRoom (RoomB, x) -> match x with | Top -> 6. | Bottom -> 7.
    | SideRoom (RoomD, Top), SideRoom (RoomC, x) -> match x with | Top -> 4. | Bottom -> 5.
    | SideRoom (RoomD, Bottom), x -> costAToB (SideRoom (RoomD, Top), x) + 1.

    // invalid moves
    | Hallway _, Hallway _
    | SideRoom (RoomA, _), SideRoom (RoomA, _)
    | SideRoom (RoomB, _), SideRoom (RoomB, _)
    | SideRoom (RoomC, _), SideRoom (RoomC, _)
    | SideRoom (RoomD, _), SideRoom (RoomD, _) -> failwithf "Invalid Move %A -> %A" locationA locationB

    // hallway to side room is just the inverse of a side room to a hallway
    | Hallway x, y -> costAToB (y, Hallway x)

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

    let amphipodAtSideRoomPos room roomPos state =
        let sideRoom = sideRoom room state
        match roomPos, sideRoom with
        | Top, x::_::[] -> Some x
        | Bottom, _::x::[] -> Some x
        | Bottom, x::[] -> Some x
        | _ -> None

    let amphipodAt location state =
        match location with
        | Hallway hallwayPos ->
            state.Hallway
            |> List.tryFind (fun (_, p) -> p = hallwayPos)
            |> Option.map fst
        | SideRoom (room, roomPos) ->
            amphipodAtSideRoomPos room roomPos state

    let sideRoomAmphipodLocations room state =
        match sideRoom room state with
        | [x] -> [ x, SideRoom (room, Bottom) ]
        | [top; bottom] -> [ top, SideRoom (room, Top)
                             bottom, SideRoom (room, Bottom) ]
        | _ -> []

    let allAmphipodLocations state =
        (state.Hallway
         |> List.map (fun (x, hallwayPos) -> x, Hallway hallwayPos))
        @ ([RoomA; RoomB; RoomC; RoomD]
          |> List.collect (fun x -> sideRoomAmphipodLocations x state))

    let removeFromHallwayAt amphipod hallwayPos state =
        state.Hallway
        |> List.tryFind (fun (_, p) -> p = hallwayPos)
        |> function
           | None -> failwithf "Nothing in hallway at: %A" hallwayPos
           | Some (a,_) as x ->
               if a <> amphipod
               then failwithf "Amphipod: %A in hallway at: %A doesn't match: %A" a hallwayPos amphipod
               else
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

    let removeFromSideRoomAt amphipod room roomPos state =
        let updateSideRoom amphipod roomPos sideRoom =
            match sideRoom, roomPos with
            | [], _ -> failwithf "Can't remove from empty sideroom: %A" room
            | [_], Top -> failwithf "Can't remove from top of room when nothing at the top. Room %A" room
            | [x], Bottom when x = amphipod -> []
            | [x], Bottom when x <> amphipod -> failwithf "Can't remove from Bottom of sideroom when non matching amphipod is present. Room %A, amphipod %A" room x
            | [y;x], Bottom  -> failwithf "Can't remove from Bottom of sideroom when Top is filled. Room %A, amphipod %A, Top %A" room x y
            | [x;y], Top when x = amphipod -> [y]
            | [x;_], Top when x <> amphipod -> failwithf "Can't remove from Top of sideroom when non matching amphipod is present. Room %A, amphipod %A" room x
            | _ -> failwithf "Invalid state, room is overfull: Room %A" room
        let sideRoom = sideRoom room state
        setSideRoom room (updateSideRoom amphipod roomPos sideRoom) state

    let addToHallway amphipod hallwayPos state =
        match state.Hallway |> List.tryFind (fun (_,p) -> p = hallwayPos) with
        | Some x -> failwithf "An amphipod: %A already exists at hallway pos %A" x hallwayPos
        | None -> { state with Hallway = (amphipod, hallwayPos)::state.Hallway }

    let removeFromState amphipod location state =
        match location with
        | Hallway hallwayPos -> removeFromHallwayAt amphipod hallwayPos state
        | SideRoom (room, roomPos) -> removeFromSideRoomAt amphipod room roomPos state

    let addToState amphipod location state =
        match location with
        | Hallway hallwayPos -> addToHallway amphipod hallwayPos state
        | SideRoom (room, roomPos) -> addToSideRoomAt amphipod room roomPos state

    let printHallway state =
        printfn "#%s%s.%s.%s.%s.%s%s#"
            (amphipodAt (Hallway FarLeft) state |> Option.defaultValue "-")
            (amphipodAt (Hallway ALeft) state |> Option.defaultValue "-")
            (amphipodAt (Hallway AB) state |> Option.defaultValue "-")
            (amphipodAt (Hallway BC) state |> Option.defaultValue "-")
            (amphipodAt (Hallway CD) state |> Option.defaultValue "-")
            (amphipodAt (Hallway DRight) state |> Option.defaultValue "-")
            (amphipodAt (Hallway FarRight) state |> Option.defaultValue "-")

    let printRooms state =
        printfn "###%s#%s#%s#%s###"
            (amphipodAt (SideRoom (RoomA, Top)) state |> Option.defaultValue "-")
            (amphipodAt (SideRoom (RoomB, Top)) state |> Option.defaultValue "-")
            (amphipodAt (SideRoom (RoomC, Top)) state |> Option.defaultValue "-")
            (amphipodAt (SideRoom (RoomD, Top)) state |> Option.defaultValue "-")

        printfn "###%s#%s#%s#%s###"
            (amphipodAt (SideRoom (RoomA, Bottom)) state |> Option.defaultValue "-")
            (amphipodAt (SideRoom (RoomB, Bottom)) state |> Option.defaultValue "-")
            (amphipodAt (SideRoom (RoomC, Bottom)) state |> Option.defaultValue "-")
            (amphipodAt (SideRoom (RoomD, Bottom)) state |> Option.defaultValue "-")

    let print state =
        printHallway state
        printRooms state

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
    pathToPosition (from, target)
    |> List.forall (fun step ->
        amphipodAtLocation state step
        |> Option.isNone)

type Move = { Amphipod: string; From : Location; To: Location }

let possibleHallwayToRoomMove state (amphipod, hallwayPos : HallwayPosition) =
    // get the sideroom this amphipod needs to be in. That's the only possible
    // room they could move to
    Some (desiredSideRoom state amphipod)
    // items in the hallway can only move into their sideroom if
    // it's empty or it contains their own type of amphipod
    |> Option.filter (fun room -> room |> List.forall (fun x -> x = amphipod))
    // items in the hallway can't move through other items in the hallway
    |> Option.map (fun room ->
        let roomPos = if room = [] then Bottom else Top
        { Amphipod = amphipod
          From = Hallway hallwayPos
          To = SideRoom (amphipodSideRoom amphipod, roomPos) })
    |> Option.filter (fun move ->
        isMoveFree state move.From move.To)

let possibleHallwayToRoomMoves state =
    state.Hallway |> List.choose (possibleHallwayToRoomMove state)

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
        |> List.map (fun pos ->
            { Amphipod = roomAmphipod
              From = (SideRoom (room, Top))
              To = (Hallway pos)})
        |> List.filter (fun move -> isMoveFree state move.From move.To)

    | otherAmphipod :: _ -> // another otherAmphipod, with potentially something underneath
        let roomPos = if List.length sideRoom = 1 then Bottom else Top
        let desiredRoom = desiredSideRoom state otherAmphipod
        let desiredRoomType = amphipodSideRoom otherAmphipod
        // get valid position in the side room
        let move =
            // if the desired room is empty, then we want to move to the bottom of that room
            if List.isEmpty desiredRoom
            then Some { Amphipod = otherAmphipod
                        From = SideRoom (room, roomPos)
                        To = SideRoom (desiredRoomType, Bottom) }
            // if the desired room has a single amphipod of the same type then we want to move to the top of that room
            elif desiredRoom = [otherAmphipod]
            then Some { Amphipod = otherAmphipod
                        From = SideRoom (room, roomPos)
                        To = SideRoom (desiredRoomType, Top) }
            // we can't move into that room, but we can always move to a hallway
            else None

        let move =
            move
            |> Option.filter (fun move -> isMoveFree state move.From move.To)

        match move with
        | Some move -> move |> List.singleton
        | None ->
            // We couldn't move the amphipod directly to the room it belonged
            // for whatever reason, so check where we can move it to in the hallway.
            // (Note: The hallway moves are always valid options, but I don't
            // think it makes any sense to consider them if a direct move would
            // have worked)
            allHallwayPositions
            |> List.map (fun pos ->
                { Amphipod = otherAmphipod
                  From = SideRoom (room, roomPos)
                  To = Hallway pos })
            |> List.filter (fun move ->
                isMoveFree state move.From move.To)

let allPossibleSideRoomMoves state =
    [ RoomA; RoomB; RoomC; RoomD ]
    |> List.collect (fun r -> possibleSideRoomMoves r state)

let neighbours state =
    let possibleMoves =
        // The neighbour states are all the possible moves
        // All items in the hallway could move to a valid side room
        possibleHallwayToRoomMoves state
        // The top items in each side room could move out into any of the
        // hallway positions, or into a valid side room
        @ allPossibleSideRoomMoves state

    possibleMoves
    |> List.map (fun move ->
        let nextState =
            state
            |> State.removeFromState move.Amphipod move.From
            |> State.addToState move.Amphipod move.To

        let cost =
            costAToB (move.From, move.To)
            |> (*) (amphipodCost move.Amphipod)

        nextState, cost)

let unfilledPosition state roomType =
    [ SideRoom (roomType, Bottom); SideRoom (roomType, Top) ]
    |> List.filter (fun loc ->
        match State.amphipodAt loc state with
        | None -> true
        | Some x -> x <> roomAmphipod roomType)

let unfilledPositions state =
    [ RoomA; RoomB; RoomC; RoomD ]
    |> List.collect (unfilledPosition state)

let unfilledAmphipods amphipodLocations =
    amphipodLocations
    |> List.filter (fun (x, loc) ->
        match loc with
        | Hallway _ -> true
        | SideRoom (room, _) -> room <> amphipodSideRoom x)

let rec heuristic (state : State) =
    // find the positions that don't already have the right amphipod
    let unfilledPositions = unfilledPositions state

    let unfilledAmphipods =
        State.allAmphipodLocations state
        |> unfilledAmphipods

    // go through each amphipod location and try to fill each unfilled position,
    // counting the steps that it takes for each one
    let _, steps =
        ((unfilledPositions, 0.), unfilledAmphipods)
        ||> List.fold (fun (unfilledPositions, stepsAcc) (amphipod, loc) ->
            let positionToFill, unfilledPositions =
                unfilledPositions
                |> List.extractFirst (fun unfilledLoc ->
                    match unfilledLoc with
                    | SideRoom (unfilledRoom, roomPos) ->
                        unfilledRoom = amphipodSideRoom amphipod
                    | Hallway _ -> false)

            let cost =
                costAToB (loc, positionToFill)
                |> (*) (amphipodCost amphipod)
            unfilledPositions, stepsAcc + cost)

    steps


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


// A* finds a path from start to goal.
// h is the heuristic function. h(n) estimates the cost to reach goal from node n.
//function A_Star(start, goal, h)
let astar start goal h =
    let rec reconstructPath cameFrom current totalPath =
        match Map.tryFind current cameFrom with
        | None -> totalPath
        | Some current -> reconstructPath cameFrom current (current::totalPath)

    // Use a Map since it's a sorted tree. Seq |> head will get you the minElement
    let rec processNeighbour openSet cameFrom (gScore : Map<_,double>) fScore current (neighbor, neighborScore) =
        // d(current,neighbor) is the weight of the edge from current to neighbor
        // tentative_gScore is the distance from start to the neighbor through current
        //tentative_gScore := gScore[current] + d(current, neighbor)
        let tentativeGScore =
            gScore
            |> Map.tryFind current
            |> Option.map (fun x -> x + neighborScore)
            |> Option.defaultValue Double.PositiveInfinity

        let neighborScore =
            gScore
            |> Map.tryFind neighbor
            |> Option.defaultValue Double.PositiveInfinity

        //if tentative_gScore < gScore[neighbor]
        if tentativeGScore < neighborScore
        then
            // This path to neighbor is better than any previous one. Record it!
            //cameFrom[neighbor] := current
            let cameFrom = cameFrom |> Map.add neighbor current
            //gScore[neighbor] := tentative_gScore
            let gScore = gScore |> Map.add neighbor tentativeGScore
            //fScore[neighbor] := tentative_gScore + h(neighbor)
            let fullPathScore = (tentativeGScore + (h neighbor))
            let items = Map.tryFind fullPathScore fScore |> Option.defaultValue Set.empty
            let fScore = fScore |> Map.add fullPathScore (items |> Set.add neighbor)
            //if neighbor not in openSet
            //    openSet.add(neighbor)
            let openSet = Set.add neighbor openSet
            //printfn "Adding neighbour with score: %A" fullPathScore
            openSet, cameFrom, gScore, fScore
        else
            openSet, cameFrom, gScore, fScore

    let rec mainLoop openSet cameFrom gScore (fScore : Map<_,Set<_>>) =
        // Note: Seq.head will grab the min key value from a map because the
        // underlying implementation of an F# map is a sorted tree.
        let fScoreHead = fScore |> Seq.head
        let current = fScoreHead.Value.MinimumElement
        let fScore =
            if Set.count fScoreHead.Value > 1
            then fScore |> Map.add fScoreHead.Key (fScoreHead.Value |> Set.remove current)
            else fScore |> Map.remove fScoreHead.Key

        //printfn "Current"
        //State.print current

        if current = goal
        then Some (reconstructPath cameFrom current [goal], Map.find goal gScore)
        elif Set.isEmpty openSet
        then None
        else
            let openSet = Set.remove current openSet
            let neighbours = neighbours current
            //printfn "Neighbours"
            //neighbours |> List.iter (fun (state, c) -> printfn "%A:" c; State.printHallway state)
            //printfn "Neighbours count: %A" (List.length neighbours)
            //printfn "fScore length: %A" (Map.count fScore)
            //printfn "openSet length: %A" (Set.count openSet)
            let openSet, cameFrom, gScore, fScore =
                ((openSet, cameFrom, gScore, fScore), neighbours)
                ||> List.fold (fun (openSet, cameFrom, gScore, fScore) neighbour ->
                    processNeighbour openSet cameFrom gScore fScore current neighbour)

            mainLoop openSet cameFrom gScore fScore

    // The set of discovered nodes that may need to be (re-)expanded.
    // Initially, only the start node is known.
    // This is usually implemented as a min-heap or priority queue rather than a hash-set.
    //openSet := {start}
    //let openSet = Set.singleton start
    let openSet = Set.singleton start // we won't add current to the set, because we changed the recursion order

    // For node n, cameFrom[n] is the node immediately preceding it on the cheapest path from start
    // to n currently known.
    //cameFrom := an empty map
    let cameFrom = Map.empty

    // For node n, gScore[n] is the cost of the cheapest path from start to n currently known.
    //gScore := map with default value of Infinity
    //gScore[start] := 0
    let gScore = Map [start, 0.]

    // For node n, fScore[n] := gScore[n] + h(n). fScore[n] represents our current best guess as to
    // how cheap a path could be from start to finish if it goes through n.
    //fScore := map with default value of Infinity
    //fScore[start] := h(start)
    let fScore = Map [(h start), Set [start]]

    mainLoop openSet cameFrom gScore fScore


let part1 (startState : State) =
    let solutionSteps, cost =
        astar startState goalState heuristic
        |> Option.get

    printfn "Solution:"
    solutionSteps
    |> List.iter (fun state ->
        printfn "---"
        State.print state)

    solutionSteps |> List.length, cost
    // Correct Answer: 15322 took: 351ms


let input = readLinesWithSlashComments "day23.txt" |> parseInput

Helper.measurePart1 part1 input

