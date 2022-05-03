#load "./Helper.fsx"
open Helper

// Day 14 - Extended Polymerization
//
// Insert a char in between each char in a string according to a key, and repeat
// it X number of times, creating an ever larger string.
//
// Part 1 - Do 10 iterations, which is fast enough to do using a naive solution
// Part 2 - Because the string is pretty much doubling after ever iteration
//          it quickly becomes too large to operate on effectively (aka the
//          parable of rice on chess board, doubling each square) so we need a
//          smarter way of doing it than string manipulation.
//
//          The solution was to only worry about char counts, and not positions,
//          (since that's all that matters in the end result), and then to
//          memoize the results so that for a given left, right and level we
//          already know the result. By level I mean how many steps/iterations
//          remaining in the insertion process.

// part 1 - naive solution using recursive string manipulation
let runInsertionStep rules (template : string) =
    ((None, []), template)
    ||> Seq.fold (fun (prev, acc) right ->
        match prev with
        | None -> (Some right, acc)
        | Some left ->
            let insert = Map.find (left, right) rules
            (Some right, insert::left::acc))
    |> fun (Some right, acc) -> right::acc // add the final dangling char
    |> Seq.rev
    |> String.fromChars

let calcMaxMinDiff str =
    let charByCount = str |> Seq.countBy id
    printfn "%s" (Seq.toString ";" charByCount)
    let max = Seq.maxBy snd charByCount |> snd
    let min = Seq.minBy snd charByCount |> snd
    max - min

// part 2 - uses memoization and Maps of character counts
let rec getCharCounts (left, right) lvl rules cache =
    if lvl = 0
    then [left; right] |> Seq.countBy id |> Map |> Map.mapValues int64, cache
    else
        match Map.tryFind (left, right, lvl) cache with
        | Some r -> r, cache
        | None ->
            let insert = Map.find (left, right) rules
            let leftCount, cache = getCharCounts (left, insert) (lvl - 1) rules cache
            let rightCount, cache = getCharCounts (insert, right) (lvl - 1) rules cache
            let merged =
                Map.merge (fun leftOpt rightOpt ->
                    defaultArg leftOpt 0L + defaultArg rightOpt 0L) leftCount rightCount
            // take care we don't double count the insert char twice in both results
            let merged = Map.add insert (Map.find insert merged - 1L) merged
            let cache = cache |> Map.add (left, right, lvl) merged
            merged, cache

let calcPart2 (template : string) rules steps =
    ((Map.empty, Map.empty), Seq.pairwise template)
    ||> Seq.fold (fun (acc, cache) (left, right) ->
        let result, cache = getCharCounts (left, right) steps rules cache
        let merged =
            Map.merge (fun leftOpt rightOpt ->
                defaultArg leftOpt 0L + defaultArg rightOpt 0L) acc result
        // take care we don't double count the left char twice in each pair
        let merged = Map.add left (Map.find left merged - 1L) merged
        merged, cache)
    |> fst
    // we removed the first char when making sure not to double count the pairwise
    // items. We need to add it back
    |> fun merged -> Map.add template.[0] (Map.find template.[0] merged + 1L) merged

let input =
    let [ template; rules ] =
        readLinesWithHashComments "day14.txt" |> splitOnEmptyLines
    let rules =
        rules
        |> List.map (fun str ->
            let [|input; insert|] = String.splitStr " -> " str
            (input.[0], input.[1]), insert.[0])
        |> Map
    template.[0], rules

let part1 (template, rules) =
    template
    |> (Seq.replicate 10 (runInsertionStep  rules) |> Seq.reduce (>>))
    |> calcMaxMinDiff
    // Correct Answer: 2584 took: 30ms

let part2 (template, rules) =
    let charByCount = calcPart2 template rules 40 |> Map.toSeq
    let max = Seq.maxBy snd charByCount |> snd
    let min = Seq.minBy snd charByCount |> snd
    max - min
    // Correct Answer: 3816397135460 took: 67ms


Helper.measurePart1 part1 input
Helper.measurePart2 part2 input
