#load "./Helper.fsx"
open Helper

// Day 18 - Snailfish
//
// Tree manipulation
//
// Remarks: Simple parser for input.
//          The hardest part was solving this immutably. it would have been a
//          fair bit easier (and faster) if I used mutable trees.
//          The only tricky part was identifying nodes via a path, and then
//          constructing a path to find the ones to either side.

type Model =
    | Pair of Model * Model
    | Value of int

type ParseStackData =
   | OpenPair
   | Comma
   | Model of Model

let rec modelToString x=
    match x with
    | Pair (left, right) -> "[" + (modelToString left)  + ","  + (modelToString right) + "]"
    | Value v -> v.ToString()

let rec parseInput (input: char list) stack =
    match input with
    | [] ->
        let Model model::[] = stack
        model
    | '['::xs -> parseInput xs (OpenPair::stack)
    | ','::xs -> parseInput xs (Comma::stack)
    | ch::xs when ch >= '0' && ch <= '9' ->
        parseInput xs ((Model (Value (Char.digitToInt ch)))::stack)
    | ']'::xs ->
        let Model right::stack = stack
        let Comma::stack = stack
        let Model left::stack = stack
        let OpenPair::stack = stack
        parseInput xs ((Model (Pair (left, right)))::stack)
    | ch::_ -> failwithf "Unexpected token: %c" ch

type PathStep = | L | R

// Result is backwards, that's beneficial for our next step so leave it
let rec findExplodingNumber model path =
    match model with
    | Value _ -> None
    | Pair (left, right) ->
        match List.length path with
        | 4 -> Some (path, (left, right))
        | x when x > 4 -> failwithf "Error cannot support nesting of more than 4 pairs"
        | _ -> findExplodingNumber left (L::path)
               |> Option.orElseWith (fun _ -> findExplodingNumber right (R::path))

let rec getPathForLeftNumber revPath =
    let rec getPathForLeftNumber' revPath =
        // the number left of the exploding number is highest level that has a left,
        // than all the way right from that
        match revPath with
        | L::revPath -> getPathForLeftNumber' revPath //need to keep searching higher
        | R::revPath ->
            // go left, then get furthest right, we may have extra right's here
            Some (R::R::R::R::L::revPath)
        | [] -> None // there is no number to the left. If we were the left most
                   // position this will happen
    getPathForLeftNumber' revPath |> Option.map List.rev

let rec getPathForRightNumber revPath =
    // same as getPathForLeftNumber, but switching left and right
    let rec getPathForRightNumber' revPath =
        match revPath with
        | L::revPath ->
            // go right, then get furthest left, we may have extra left's here
            Some (L::L::L::L::R::revPath)
        | R::revPath -> getPathForRightNumber' revPath //need to keep searching higher
        | [] -> None
    getPathForRightNumber' revPath |> Option.map List.rev

let rec updatePath model path update =
    match model with
    | Pair (left, right) ->
        match path with
        | [] ->
            // if we've reached the end of the path we've found the model we want
            update model
        | L::path -> Pair (updatePath left path update, right)
        | R::path -> Pair (left, updatePath right path update)
    | Value _ ->
        // We may not have reached the end of the path, but since this is the
        // only viable item, we update it
        update model

let updateValue model path carryValue =
    match path with
    | None -> model
    | Some path ->
        updatePath model path (function
            | Value v -> Value (v + carryValue)
            | Pair _ -> failwith "Expecting a value")

let performExplosionStep model =
    match findExplodingNumber model [] with
    | None -> None // there was no explosion
    | Some (revPath, (Value left, Value right)) ->
        let path = revPath |> List.rev // path was backwards, fix it
        let pathForLeftUpdate = getPathForLeftNumber revPath
        let pathForRightUpdate = getPathForRightNumber revPath

        let model = updateValue model pathForLeftUpdate left
        let model = updateValue model pathForRightUpdate right

        // need to replace the exploded item last, otherwise it affects the
        // paths for the left and right udpate
        let model = updatePath model path (fun _ -> Value 0)
        Some model

let rec findNumberToSplit model =
    match model with
    | Value x -> if x >= 10 then Some x else None
    | Pair (left, right) ->
        findNumberToSplit left
        |> Option.orElseWith (fun _ -> findNumberToSplit right)

type SplitStatus = | HasSplit | HasNotSplit

let rec splitIfAble model =
    match model with
    | Value x ->
        if x < 10
        then HasNotSplit, Value x
        else
            let mid = (float x) / 2.
            HasSplit, Pair (Value (int (floor mid)), Value (int (ceil mid)))
    | Pair (left, right) ->
        match splitIfAble left with
        | HasSplit, left -> HasSplit, Pair (left, right)
        | HasNotSplit, _ ->
            match splitIfAble right with
            | HasSplit, right -> HasSplit, Pair (left, right)
            | HasNotSplit, _ -> HasNotSplit, Pair (left, right)

let rec reduceNumber model =
    match performExplosionStep model with
    | Some model -> reduceNumber model
    | None ->
        match splitIfAble model with
        | HasSplit, model -> reduceNumber model
        | HasNotSplit, _ -> model // we didn't explode or split, so nothing to reduce

let rec calculateMagnitude model =
    match model with
    | Value x -> x
    | Pair (left, right) ->
        3 * (calculateMagnitude left) + 2 * (calculateMagnitude right)

let rec processInput models =
    match models with
    | [] -> failwith "No input to process"
    | model::[] -> reduceNumber model
    | modelA::modelB::xs ->
        let model = Pair (modelA, modelB) // the addition
        processInput (reduceNumber model::xs)

let input =
    readLinesWithHashComments "day18.txt"
    |> Seq.map (fun xs -> parseInput (Seq.toList xs) [])
    |> Seq.toList

let part1 homework =
    processInput homework
    |> calculateMagnitude
    // Correct Answer: 3675 took: 34ms

let crossJoin xs = xs |> List.collect(fun a -> xs |> List.map (mkTuple a))

let part2 homework =
    homework
    |> crossJoin
    |> List.map (fun (a, b) -> processInput [a;b] |> calculateMagnitude)
    |> List.max
    // Correct Answer: 4650 took: 351ms

Helper.measurePart1 part1 input
Helper.measurePart2 part2 input
