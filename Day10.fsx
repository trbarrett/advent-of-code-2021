#load "./Helper.fsx"
open Helper

// Day 10 - Matching delimiters
//
// Part 1 & 2 - Simple stack symbol operations

let openSymbol = Set [ '('; '['; '{'; '<' ]
let matchingClose = function | '(' -> ')' | '[' -> ']' | '{' -> '}' | '<' -> '>'
let part1SymbolValue = function | ')' -> 3 | ']' -> 57 | '}' -> 1197 | '>' -> 25137
let part2SymbolValue = function | ')' -> 1L | ']' -> 2L | '}' -> 3L | '>' -> 4L

type ParseResult =
    | CompleteLine | InvalidSymbol of int | IncompleteLine of char list

let parseLine line =
    let rec find rest stack =
        match rest, stack with
        | [], [] -> CompleteLine
        | [], _::_  -> IncompleteLine stack
        | next::rest, _ when Set.contains next openSymbol -> find rest (next::stack)
        | next::rest, top::stack when next = matchingClose top -> find rest stack
        | next::_, _::_ -> InvalidSymbol (part1SymbolValue next)
        | _, [] -> failwithf "WTF. rest: %s" (Seq.toString "" rest)
    find line []

let navigationSubsystem =
    readLinesWithHashComments "day10.txt" |> List.ofSeq |> List.map Seq.toList

let part1 navigationSubsystem =
    navigationSubsystem |> List.map parseLine
    |> List.choose (function | InvalidSymbol x -> Some x | _ -> None)
    |> List.sum
    // Correct Answer: 271245 took: 3ms

let part2 navigationSubsystem =
    navigationSubsystem |> List.map parseLine
    |> List.choose (function | IncompleteLine xs -> Some xs | _ -> None)
    |> List.map (fun remaining ->
        (0L, remaining)
        ||> List.fold (fun acc x -> acc * 5L + part2SymbolValue (matchingClose x)))
    |> List.sortDescending
    |> (fun xs -> List.item (List.length xs / 2) xs)
    // Correct Answer: 1685293086 took: 6ms

Helper.measurePart1 part1 navigationSubsystem
Helper.measurePart2 part2 navigationSubsystem