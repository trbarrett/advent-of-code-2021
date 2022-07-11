#load "./Helper.fsx"
#load "./Day23.fsx"
#r "./bin/Debug/netcoreapp3.1/Expecto.dll"
open Helper
open Day23
open Expecto
open Expecto.Flip

let state = readLinesWithSlashComments "day23-example1.txt" |> parseInput


[<Tests>]
let parseInputTest =
    testList "Parse Input Tests" [
        testCase "Parse Input" <| fun _ ->
            let expect =
                { SideRoomA = [ "B"; "A" ]
                  SideRoomB = [ "C"; "D" ]
                  SideRoomC = [ "B"; "C" ]
                  SideRoomD = [ "D"; "A" ]
                  Hallway   = [] }

            state |> Expect.equal "" expect
    ]

let stateTests =
    testList "State Tests" [
        testCase "Amphipod At" <| fun _ ->
            let test loc amphipod =
                State.amphipodAt loc state |> Expect.equal "" amphipod

            test (SideRoom (RoomA, Top)) (Some "B")
            test (SideRoom (RoomA, Bottom)) (Some "A")
            test (SideRoom (RoomB, Top)) (Some "C")
            test (SideRoom (RoomB, Bottom)) (Some "D")
            test (SideRoom (RoomC, Top)) (Some "B")
            test (SideRoom (RoomC, Bottom)) (Some "C")
            test (SideRoom (RoomD, Top)) (Some "D")
            test (SideRoom (RoomD, Bottom)) (Some "A")

        testCase "Remove From SideRoom At" <| fun _ ->
            let state = state |> State.removeFromSideRoomAt "B" RoomA Top
            state.SideRoomA |> Expect.equal "" ["A"]

            let state = state |> State.removeFromSideRoomAt "A" RoomA Bottom
            state.SideRoomA |> Expect.equal "" []

            let state = state |> State.removeFromSideRoomAt "B" RoomC Top
            state.SideRoomC |> Expect.equal "" ["C"]

        testCase "All Amphipod Locations" <| fun _ ->
            let state =
                state
                |> State.addToHallway "B" ALeft
                |> State.removeFromSideRoomAt "B" RoomA Top

            let expected =
                [ "B", Hallway ALeft
                  "A", SideRoom (RoomA, Bottom)
                  "C", SideRoom (RoomB, Top); "D", SideRoom (RoomB, Bottom)
                  "B", SideRoom (RoomC, Top); "C", SideRoom (RoomC, Bottom)
                  "D", SideRoom (RoomD, Top); "A", SideRoom (RoomD, Bottom) ]

            State.allAmphipodLocations state
            |> Expect.equal "" expected
    ]

[<Tests>]
let pathingTests =
    testList "Path Tests" [
        testCase "pathToPosition" <| fun _ ->
            pathToPosition (SideRoom (RoomA, Top)) (Hallway ALeft)
            |> Expect.equal "" [ Hallway ALeft ]
            pathToPosition (SideRoom (RoomA, Top)) (Hallway FarRight)
            |> Expect.equal "" [ Hallway AB; Hallway BC; Hallway CD
                                 Hallway DRight; Hallway FarRight ]
            pathToPosition (SideRoom (RoomC, Bottom)) (Hallway FarLeft)
            |> Expect.equal "" [ SideRoom (RoomC, Top); Hallway BC; Hallway AB
                                 Hallway ALeft; Hallway FarLeft ]
            pathToPosition (Hallway FarLeft) (SideRoom (RoomC, Bottom))
            |> Expect.equal "" [ Hallway ALeft; Hallway AB; Hallway BC
                                 SideRoom (RoomC, Top); SideRoom (RoomC, Bottom) ]
            pathToPosition (SideRoom (RoomB, Bottom)) (SideRoom (RoomD, Bottom))
            |> Expect.equal "" [ SideRoom (RoomB, Top); Hallway BC; Hallway CD
                                 SideRoom (RoomD, Top); SideRoom (RoomD, Bottom) ]

        testCase "isMoveFree" <| fun _ ->
            isMoveFree state (SideRoom (RoomA, Top)) (Hallway ALeft)
            |> Expect.isTrue ""

            isMoveFree state (SideRoom (RoomA, Bottom)) (Hallway ALeft)
            |> Expect.isFalse ""

            let state =
                state
                |> State.addToHallway "B" ALeft
                |> State.removeFromSideRoomAt "B" RoomA Top

            isMoveFree state (SideRoom (RoomA, Bottom)) (Hallway AB)
            |> Expect.isTrue ""

            isMoveFree state (SideRoom (RoomA, Bottom)) (Hallway FarLeft)
            |> Expect.isFalse ""

            let state =
                state
                |> State.addToHallway "C" BC
                |> State.removeFromSideRoomAt "C" RoomB Top

            isMoveFree state (Hallway AB) (SideRoom (RoomB, Top))
            |> Expect.isTrue ""

    ]

[<Tests>]
let neighboursTest =
    testList "Neighbours Tests" [
        testCase "Possible Side Room Moves" <| fun _ ->
            possibleSideRoomMoves RoomA state
            |> Expect.notEqual "" []
    ]

[<Tests>]
let heuristicsTest =
    testList "Heuristics Tests" [
        testCase "Unfilled Positions" <| fun _ ->
            let expected =
               [ SideRoom (RoomA, Top)
                 SideRoom (RoomB, Bottom)
                 SideRoom (RoomB, Top)
                 SideRoom (RoomC, Top)
                 SideRoom (RoomD, Bottom) ]

            unfilledPositions state
            |> Expect.equal "" expected

        testCase "Unfilled Amphipods" <| fun _ ->
            let expected =
                [ "B", SideRoom (RoomA, Top)
                  "C", SideRoom (RoomB, Top); "D", SideRoom (RoomB, Bottom)
                  "B", SideRoom (RoomC, Top)
                  "A", SideRoom (RoomD, Bottom) ]

            State.allAmphipodLocations state
            |> unfilledAmphipods
            |> Expect.equal "" expected

        testCase "heuristicsTest" <| fun _ ->
            //Room 1: B -> 5 (Top -> Bottom)
            //Room 2: C -> 4 (Top -> Top)
            //Room 2: D -> 8 (Bottom -> Bottom)
            //Room 3: B -> 4 (Top -> Top)
            //Room 4: A -> 9 (Bottom -> Top)
            //           = 30
            heuristic state
            |> Expect.equal "" 30.0
    ]

// path test

let testList = testList "Day22Tests" [ parseInputTest; stateTests; heuristicsTest; neighboursTest; pathingTests ]

runTestsWithCLIArgs [] [||] testList
