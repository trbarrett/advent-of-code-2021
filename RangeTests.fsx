#load "./Helper.fsx"
#load "./Range.fsx"
#r "./bin/Debug/netcoreapp3.1/Expecto.dll"
open Range
open Expecto
open Expecto.Flip


[<Tests>]
let rangeTest =
    testList "Range Tests" [
        testList "fromValues" [
            testCase "should have empty response for empty input" <| fun _ ->
                [ 1L; 2L; 3L; 4L ]
                |> Range.fromValues
                |> Expect.equal "" [ { From = 1L; To = 4L } ]

            testCase "should have one range when all in sequence" <| fun _ ->
                [ 1L; 2L; 3L; 4L ]
                |> Range.fromValues
                |> Expect.equal "" [ { From = 1L; To = 4L } ]

            testCase "should have one range when all in sequence when unordered" <| fun _ ->
                [ 3L; 2L; 4L; 1L ]
                |> Range.fromValues
                |> Expect.equal "" [ { From = 1L; To = 4L } ]

            testCase "should have one range when all in sequence neg and positive" <| fun _ ->
                [ -5L; -4L; -3L; -2L; -1L; 0L; 1L; 2L; 3L ]
                |> Range.fromValues
                |> Expect.equal "" [ { From = -5L; To = 3L } ]

            testCase "should have range for each value when all discontinuous" <| fun _ ->
                [ 1L; 3L; 5L; 7L ]
                |> Range.fromValues
                |> Expect.equal "" [ { From = 1L; To = 1L }
                                     { From = 3L; To = 3L }
                                     { From = 5L; To = 5L }
                                     { From = 7L; To = 7L } ]

            testCase "should break into separate ranges when values are separated" <| fun _ ->
                [ 0L; 1L; 2L; 3L; 55L; 56L; 57L ]
                |> Range.fromValues
                |> Expect.equal "" [ { From = 0L; To = 3L }
                                     { From = 55L; To = 57L } ]
        ]

        testList "intersect" [
            testCase "when both 0" <| fun _ ->
                Range.tryIntersect
                    { From = 0L; To = 0L }
                    { From = 0L; To = 0L }
                |> Expect.equal "" (Some { From = 0L; To = 0L } )

            testCase "when aFrom < bFrom and they intersect" <| fun _ ->
                Range.tryIntersect
                    { From = -1L; To = 8L }
                    { From = 4L; To = 14L }
                |> Expect.equal "" (Some { From = 4L; To = 8L } )

            testCase "when a fully covers b" <| fun _ ->
                Range.tryIntersect
                    { From = -1L; To = 8L }
                    { From = 1L; To = 5L }
                |> Expect.equal "" (Some { From = 1L; To = 5L } )

            testCase "when bFrom < aFrom and they intersect" <| fun _ ->
                Range.tryIntersect
                    { From = 12L; To = 22L }
                    { From = -10L; To = 18L }
                |> Expect.equal "" (Some { From = 12L; To = 18L } )

            testCase "when b fully covers a" <| fun _ ->
                Range.tryIntersect
                    { From = 12L; To = 22L }
                    { From = -10L; To = 33L }
                |> Expect.equal "" (Some { From = 12L; To = 22L } )

            testCase "when a is completely before b" <| fun _ ->
                Range.tryIntersect
                    { From = -12L; To = 2L }
                    { From = 10L; To = 33L }
                |> Expect.equal "" None

            testCase "when b is completely before a" <| fun _ ->
                Range.tryIntersect
                    { From = 14L; To = 33L }
                    { From = 2L; To = 9L }
                |> Expect.equal "" None
        ]

        testList "union" [
            testCase "when both 0" <| fun _ ->
                Range.tryUnion
                    { From = 0L; To = 0L }
                    { From = 0L; To = 0L }
                |> Expect.equal "" (Some { From = 0L; To = 0L } )

            testCase "when aFrom < bFrom and they intersect" <| fun _ ->
                Range.tryUnion
                    { From = -1L; To = 8L }
                    { From = 4L; To = 14L }
                |> Expect.equal "" (Some { From = -1L; To = 14L } )

            testCase "when a fully covers b" <| fun _ ->
                Range.tryUnion
                    { From = -1L; To = 8L }
                    { From = 1L; To = 5L }
                |> Expect.equal "" (Some { From = -1L; To = 8L } )

            testCase "when bFrom < aFrom and they intersect" <| fun _ ->
                Range.tryUnion
                    { From = 12L; To = 22L }
                    { From = -10L; To = 18L }
                |> Expect.equal "" (Some { From = -10L; To = 22L } )

            testCase "when b fully covers a" <| fun _ ->
                Range.tryUnion
                    { From = 12L; To = 22L }
                    { From = -10L; To = 33L }
                |> Expect.equal "" (Some { From = -10L; To = 33L } )

            testCase "when a is completely before b" <| fun _ ->
                Range.tryUnion
                    { From = -12L; To = 2L }
                    { From = 10L; To = 33L }
                |> Expect.equal "" None

            testCase "when b is completely before a" <| fun _ ->
                Range.tryUnion
                    { From = 14L; To = 33L }
                    { From = 2L; To = 9L }
                |> Expect.equal "" None
        ]

        testList "simplify" [
            testCase "simplify when all 0" <| fun _ ->
                [ { From = 0L; To = 0L }
                  { From = 0L; To = 0L }
                  { From = 0L; To = 0L } ]
                |> Range.simplify
                |> Expect.equal "" [ { From = 0L; To = 0L } ]

            testCase "simplify when all the same" <| fun _ ->
                [ { From = 2L; To = 8L }
                  { From = 2L; To = 8L }
                  { From = 2L; To = 8L } ]
                |> Range.simplify
                |> Expect.equal "" [ { From = 2L; To = 8L } ]

            testCase "simplify when all overlapping" <| fun _ ->
                [ { From = -3L; To = 4L }
                  { From = 2L; To = 8L }
                  { From = 4L; To = 13L }
                  { From = 8L; To = 15L }
                  { From = 12L; To = 22L } ]
                |> Range.simplify
                |> Expect.equal "" [ { From = -3L; To = 22L } ]

            testCase "simplify when overlapping exactly" <| fun _ ->
                [ { From = -3L; To = 4L }
                  { From = 4L; To = 12L }
                  { From = 12L; To = 22L } ]
                |> Range.simplify
                |> Expect.equal "" [ { From = -3L; To = 22L } ]

            testCase "simplify when some overlapping" <| fun _ ->
                [ { From = -3L; To = 4L }
                  { From = 2L; To = 8L }
                  { From = 10L; To = 15L }
                  { From = 12L; To = 22L }
                  { From = 20L; To = 26L }
                  { From = 30L; To = 50L } ]
                |> Range.simplify
                |> Expect.equal "" [ { From = -3L; To = 8L }
                                     { From = 10L; To = 26L }
                                     { From = 30L; To = 50L } ]

            testCase "simplify negatives overlapping" <| fun _ ->
                [ { From = -50L; To = -30L }
                  { From = -26L; To = -20L }
                  { From = -22L; To = -12L }
                  { From = -15L; To = -10L }
                  { From = -8L; To = -2L }
                  { From = -4L; To = 3L } ]
                |> Range.simplify
                |> Expect.equal "" [ { From = -50L; To = -30L }
                                     { From = -26L; To = -10L }
                                     { From = -8L;  To = 3L } ]
        ]

        testList "valueDiv" [
            testCase "should divide positive by positives" <| fun _ ->
                Range.valueDiv 20 { From = 3L; To = 30L }
                |> Expect.equal "" { From = 0L; To = 6L }

            testCase "should divide positive by range crossing 0" <| fun _ ->
                Range.valueDiv 20 { From = -4L; To = 30L }
                |> Expect.equal "" { From = -20L; To = 20L }
        ]

        testList "div" [
            testCase "should divide positive by positives" <| fun _ ->
                Range.div { From = 2L; To = 110L } { From = 3L; To = 30L }
                |> Expect.equal "" { From = 0L; To = 36L }

            testCase "should divide positive range by range crossing 0" <| fun _ ->
                Range.div { From = 2L; To = 110L } { From = -4L; To = 30L }
                |> Expect.equal "" { From = -110L; To = 110L }

            testCase "should divide positive range by negative range" <| fun _ ->
                Range.div { From = 2L; To = 110L } { From = -20L; To = -8L }
                |> Expect.equal "" { From = -13L; To = 0L }

            testCase "should divide negative range by negative range" <| fun _ ->
                Range.div { From = -82L; To = -12L } { From = -20L; To = -8L }
                |> Expect.equal "" { From = 0L; To = 10L }

            testCase "should divide range crossing 0 by range crossing 0" <| fun _ ->
                Range.div { From = -88L; To = 42L } { From = -4L; To = 30L }
                |> Expect.equal "" { From = -88L; To = 88L }

            testCase "should divide with 0 to 1 range" <| fun _ ->
                Range.div { From = 8L; To = 16L } { From = 0L; To = 1L }
                |> Expect.equal "" { From = 8L; To = 16L }

            testCase "should divide with 0 to 6 range" <| fun _ ->
                Range.div { From = 8L; To = 16L } { From = 0L; To = 6L }
                |> Expect.equal "" { From = 1L; To = 16L }

            testCase "should divide with -6 to 0 range" <| fun _ ->
                Range.div { From = 8L; To = 16L } { From = -6L; To = 0L }
                |> Expect.equal "" { From = -16L; To = -1L }
        ]

        testList "modValue" [
            testCase "should have empty list when value below 1" <| fun _ ->
                Range.modValue { From = 2L; To = 30L } 0L
                |> Expect.equal "" [ ]

            testCase "should have empty list when range below 0" <| fun _ ->
                Range.modValue { From = -3L; To = -1L } 30L
                |> Expect.equal "" [ ]

            testCase "should have partial result when range cross 0" <| fun _ ->
                Range.modValue { From = -3L; To = 3L } 30L
                |> Expect.equal "" [ { From = 0L; To = 3L } ]

            testCase "should have partial result when range is 0" <| fun _ ->
                Range.modValue { From = -3L; To = 0L } 30L
                |> Expect.equal "" [ { From = 0L; To = 0L } ]

            testCase "should mod range by value, resulting in single range" <| fun _ ->
                Range.modValue { From = 2L; To = 110L } 30L
                |> Expect.equal "" [ { From = 0L; To = 29L } ]

            testCase "should mod range by value, resulting in two ranges" <| fun _ ->
                Range.modValue { From = 24L; To = 42L } 30L
                |> Expect.equal "" [ { From = 24L; To = 29L }
                                     { From =  0L; To = 12L } ]
        ]

        testList "valueMod" [
            testCase "should have empty list when value below 0" <| fun _ ->
                Range.valueMod -1L { From = 2L; To = 30L }
                |> Expect.equal "" [ ]

            testCase "should have empty list when range below 1" <| fun _ ->
                Range.valueMod 30L { From = -3L; To = 0L }
                |> Expect.equal "" [ ]

            testCase "should have partial result when range cross 0" <| fun _ ->
                Range.valueMod 30L { From = -3L; To = 3L }
                |> Expect.equal "" [ { From = 0L; To = 0L } ]

            testCase "should mod range by value, resulting in multiple ranges" <| fun _ ->
                Range.valueMod 30L { From = 2L; To = 14L }
                |> Expect.equal "" [ { From = 0L; To = 0L }
                                     { From = 2L; To = 4L }
                                     { From = 6L; To = 6L }
                                     { From = 8L; To = 8L }]
        ]

        testList "mod" [
            testCase "should have empty list when value below 0" <| fun _ ->
                Range.modRanges { From = -20L; To = -1L } { From = 2L; To = 30L }
                |> Expect.equal "" [ ]

            testCase "should have empty list when range below 1" <| fun _ ->
                Range.modRanges { From = 2L; To = 30L } { From = -3L; To = 0L }
                |> Expect.equal "" [ ]

            testCase "should have partial result when range cross 0" <| fun _ ->
                Range.modRanges { From = 2L; To = 30L } { From = -3L; To = 3L }
                |> Expect.equal "" [ { From = 0L; To = 2L } ]

            testCase "should have multiple ranges" <| fun _ ->
                Range.modRanges { From = 13L; To = 16L } { From = 4L; To = 7L }
                |> Expect.equal "" [ { From = 0L; To = 4L }
                                     { From = 6L; To = 6L } ]
        ]
    ]

runTestsWithCLIArgs [] [||] rangeTest
