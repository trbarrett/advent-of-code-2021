#load "./Helper.fsx"
#load "./Day22.fsx"
#r "./bin/Debug/netcoreapp3.1/Expecto.dll"
open Helper
open Day22
open Expecto
open Expecto.Flip

[<Tests>]
let cuboidSubtractTests =
    testList "Cuboid subtract tests" [
        testCase "Should subtract when single axis intersection and one side (X)" <| fun _ ->
            let results =
                Cuboid.subtract ((8,12), (10,20), (10,20))
                                ((10,20), (10,20), (10,20))

            results
            |> Expect.equal "" [(8,9), (10,20), (10,20)]

        testCase "Should subtract when single axis subset and one side (X)" <| fun _ ->
            let results =
                Cuboid.subtract ((8,20), (10,20), (10,20))
                                ((10,20), (10,20), (10,20))

            results
            |> Expect.equal "" [(8,9), (10,20), (10,20)]

        testCase "Should subtract when single axis and two sides (X)" <| fun _ ->
            let results =
                Cuboid.subtract ((8,22), (10,20), (10,20))
                                ((10,20), (10,20), (10,20))

            results
            |> Expect.equal "" [(8,9), (10,20), (10,20)
                                (21,22), (10,20), (10,20)]

        testCase "Should subtract when single axis intersection and one side (Y)" <| fun _ ->
            let results =
                Cuboid.subtract ((10,20), (8,12), (10,20))
                                ((10,20), (10,20), (10,20))

            results
            |> Expect.equal "" [(10,20), (8,9), (10,20)]

        testCase "Should subtract when single axis subset and one side (Y)" <| fun _ ->
            let results =
                Cuboid.subtract ((10,20), (8,20), (10,20))
                                ((10,20), (10,20), (10,20))

            results
            |> Expect.equal "" [(10,20), (8,9), (10,20)]

        testCase "Should subtract when single axis and two sides (Y)" <| fun _ ->
            let results =
                Cuboid.subtract ((10,20), (8,22), (10,20))
                                ((10,20), (10,20), (10,20))

            results
            |> Expect.equal "" [(10,20), (8,9), (10,20)
                                (10,20), (21,22), (10,20)]

        testCase "Should subtract when single axis intersection and one side (Z)" <| fun _ ->
            let results =
                Cuboid.subtract ((10,20), (10,20), (8,12))
                                ((10,20), (10,20), (10,20))

            results
            |> Expect.equal "" [(10,20), (10,20), (8,9)]

        testCase "Should subtract when single axis subset and one side (Z)" <| fun _ ->
            let results =
                Cuboid.subtract ((10,20), (10,20), (8,20))
                                ((10,20), (10,20), (10,20))

            results
            |> Expect.equal "" [(10,20), (10,20), (8,9)]

        testCase "Should subtract when single axis and two sides (Z)" <| fun _ ->
            let results =
                Cuboid.subtract ((10,20), (10,20), (8,22))
                                ((10,20), (10,20), (10,20))

            results
            |> Expect.equal "" [(10,20), (10,20), (8,9)
                                (10,20), (10,20), (21,22)]

        testCase "Should subtract when two axis subsets and one side each (X,Y)" <| fun _ ->
            let results =
                Cuboid.subtract ((8,20), (5,20), (10,20))
                                ((10,20), (10,20), (10,20))

            results
            |> Expect.equal "" [(8,9), (5,20), (10,20)
                                (10,20), (5,9), (10,20)]

        testCase "Should subtract when two axis intersections and one side each (X,Y)" <| fun _ ->
            let results =
                Cuboid.subtract ((8,12), (5,15), (10,20))
                                ((10,20), (10,20), (10,20))

            results
            |> Expect.equal "" [(8,9), (5,15), (10,20)
                                (10,12), (5,9), (10,20)]

        testCase "Should subtract when two axis subsets and two sides each (X,Y)" <| fun _ ->
            let results =
                Cuboid.subtract ((8,22), (5,25), (10,20))
                                ((10,20), (10,20), (10,20))

            results
            |> Expect.equal "" [(8,9), (5,25), (10,20)
                                (21,22), (5,25), (10,20)
                                (10,20), (5,9), (10,20)
                                (10,20), (21,25), (10,20)]

        testCase "Should subtract when 3 axis intersection and 1 side each negative" <| fun _ ->
            let results =
                Cuboid.subtract ((10,12), (10,12), (10,12))
                                ((11,13), (11,13), (11,13))

            results
            |> Expect.equal "" [(10, 10), (10,12), (10,12)
                                (11, 12), (10,10), (10,12)
                                (11, 12), (11,12), (10,10)]


        testCase "Should subtract when 3 axis intersection and 1 side each positive" <| fun _ ->
            let results =
                Cuboid.subtract ((11,13), (11,13), (11,13))
                                ((9,11), (9,11), (9,11))

            results
            |> Expect.equal "" [(12, 13), (11,13), (11,13)
                                (11, 11), (12,13), (11,13)
                                (11, 11), (11,11), (12,13)]
    ]


[<Tests>]
let dimExtentsTests =
    testList "Dim extents tests" [
        testCase "should be MutuallyExclusive when separate" <| fun _ ->
            DimExtents.relation (8,8) (10,10)
            |> Expect.equal "" MutuallyExclusive
            DimExtents.relation (8,9) (10,11)
            |> Expect.equal "" MutuallyExclusive

        testCase "should be Equal when same" <| fun _ ->
            DimExtents.relation (8,10) (8,10)
            |> Expect.equal "" Equal
            DimExtents.relation (8,8) (8,8)
            |> Expect.equal "" Equal

        testCase "should be BSubsetOfA" <| fun _ ->
            DimExtents.relation (8,12) (9,11)
            |> Expect.equal "" BSubsetOfA
            DimExtents.relation (8,12) (8,10)
            |> Expect.equal "" BSubsetOfA
            DimExtents.relation (8,12) (10,12)
            |> Expect.equal "" BSubsetOfA
            DimExtents.relation (8,12) (8,8)
            |> Expect.equal "" BSubsetOfA
            DimExtents.relation (8,12) (12,12)
            |> Expect.equal "" BSubsetOfA

        testCase "should be ASubsetOfB" <| fun _ ->
            DimExtents.relation (9,11) (8,12)
            |> Expect.equal "" ASubsetOfB
            DimExtents.relation (8,10) (8,12)
            |> Expect.equal "" ASubsetOfB
            DimExtents.relation (10,12) (8,12)
            |> Expect.equal "" ASubsetOfB
            DimExtents.relation (8,8) (8,12)
            |> Expect.equal "" ASubsetOfB
            DimExtents.relation (12,12) (8,12)
            |> Expect.equal "" ASubsetOfB

        testCase "should be Intersection" <| fun _ ->
            DimExtents.relation (8,12) (6,10)
            |> Expect.equal "" Intersection
            DimExtents.relation (6,10) (8,12)
            |> Expect.equal "" Intersection
            DimExtents.relation (6,8) (8,12)
            |> Expect.equal "" Intersection
            DimExtents.relation (8,12) (6,8)
            |> Expect.equal "" Intersection
    ]

let testList = testList "Day22Tests" [ cuboidSubtractTests; dimExtentsTests ]

runTestsWithCLIArgs [] [||] testList
