#I __SOURCE_DIRECTORY__

open System.Text.RegularExpressions
open System.IO
open System.Collections.Generic


let flip f a b = f b a
let mkTuple x y = x, y
let tee f x = f x; x

let startStopwatch () = System.Diagnostics.Stopwatch.StartNew ()

let measurePart partN f input =
    let sw = startStopwatch ()
    let result = f input
    printfn $"Part {partN} result: {result} took: {sw.ElapsedMilliseconds}ms"

let measurePart1 f input = measurePart 1 f input
let measurePart2 f input = measurePart 2 f input

let public readLinesWithHashComments inputName =
    sprintf "%s/inputdata/%s" __SOURCE_DIRECTORY__ inputName
    |> File.ReadLines
    |> Seq.filter (fun (str : System.String) -> not (str.StartsWith('#')))

let public readLinesWithSlashComments inputName =
    sprintf "%s/inputdata/%s" __SOURCE_DIRECTORY__ inputName
    |> File.ReadLines
    |> Seq.filter (fun (str : System.String) -> not (str.StartsWith("//")))

let splitOnEmptyLines lines =
    (lines, [[]])
    ||> Seq.foldBack (fun line acc ->
        match line, acc with
        | "", _ -> []::acc
        | _, curr::rest -> (line::curr)::rest
        | _ -> failwith "Not Possible")


let (|KeyValue|) (keyValuePair : KeyValuePair<'k, 'v>) : 'k * 'v =
    let k = keyValuePair.Key
    let v = keyValuePair.Value
    (k, v)

module Tuple =
    let flip (x,y) = y, x

module Map =
    let extendListValue key value m =
        match m |> Map.tryFind key with
        | None -> m |> Map.add key [value]
        | Some existing -> m |> Map.add key (value::existing)

    let ofOneToManySeq xs : Map<'a, 'b list> =
        Seq.fold (fun m (x,y) -> m |> extendListValue x y) Map.empty xs

    let flattenOneToMany m =
        let flatten = (fun (KeyValue(x, ys)) -> ys |> Seq.map (mkTuple x))
        Seq.collect flatten m

    let keys map =
        map |> Map.toSeq |> Seq.map fst

    let mapValues f m =
       m |> Map.map (fun _ v -> f v)

    let merge f mapA mapB =
       let allKeys = [mapA; mapB] |> Seq.collect keys |> Seq.distinct

       (Map [], allKeys)
       ||> Seq.fold (fun acc key ->
           let aValue = mapA |> Map.tryFind key
           let bValue = mapB |> Map.tryFind key
           acc |> Map.add key (f aValue bValue))

    let mergeAll f maps =
       let allKeys = maps |> Seq.collect keys |> Seq.distinct

       (Map [], allKeys)
       ||> Seq.fold (fun acc key ->
           // find the values in all the maps for a given key and combine them
           // with function f
           let values = maps |> Seq.map (Map.tryFind key)
           acc |> Map.add key (f values))

module List =
    let permutationsWithReplacement (values : 'a list) times =
        let splitValues = values |> List.map List.singleton
        let folder acc values =
            List.allPairs acc values
            |> List.map (fun (xs, x) -> x::xs)
        List.fold folder splitValues (List.replicate (times - 1) values)

    let rec permutations (values : Set<'a>) count : 'a list list =
        if count = 0 then [[]]
        else
            values |> List.ofSeq
            |> List.collect (fun x ->
               permutations (Set.remove x values) (count - 1)
               |> List.map (fun xs -> x::xs))

    let rec combinations n l =
        match n, l with
        | 0, _ -> [[]]
        | _, [] -> []
        | k, (x::xs) ->
            List.map ((@) [x]) (combinations (k-1) xs) @ combinations k xs

    let replaceAt replaceAt replacement xs =
        xs
        |> List.mapi (fun i x ->
            if i = replaceAt then replacement
            else x)

    /// Given a list of tuples, and treating each tuple as key/value pairs,
    /// this function will combine the values where there are duplicate pairs
    /// with the same key
    let combineKeyValuePairs reducer lst =
        lst
        |> List.groupBy fst
        |> List.map (fun (key, grp) ->
            key, (grp |> List.map snd |> List.reduce reducer ))


module Seq =
    let groupByTuple (xs : ('a * 'b) seq) =
        xs
        |> Seq.groupBy fst
        |> Seq.map (fun (k,v) -> k, v |> Seq.map snd)
        |> Map

    let toString (separator : string) (s : seq<'a>) =
        System.String.Join(separator, s)


module String =

    let fromChars (chars : seq<char>) : string =
     new System.String(Array.ofSeq chars)

    let split (delimiter : char) (input : string) =
        input.Split delimiter

    let splitStr (delimiter : string) (input : string) =
        input.Split delimiter

    let trim (input : string) = input.Trim()

    let splitIntoMatching regexPattern (input : string) =
        Regex.Matches(input, regexPattern)
        |> Seq.map (fun x -> x.Value)
        |> List.ofSeq

    let capture regexPattern (input : string) =
        Regex.Match(input, regexPattern).Groups
        |> Seq.skip 1
        |> Seq.map (fun x -> x.Value)
        |> List.ofSeq

module Char =
    let digitToInt (c : char) = int c - int '0'

module Math =
    /// Takes a seq of int64 digits and converts them into a single number
    /// based on position. e.g. [ 4L; 5L; 7L; 2L; 7L ] -> 45727L
    let digitsToInt64 digits =
        digits
        |> Seq.rev
        |> Seq.mapi (fun i x -> (int64 (pown 10 i)) * x )
        |> Seq.sum

    /// Takes a seq of int64 digits and converts them into a single number
    /// based on position. e.g. [ 4; 5; 7; 2; 7 ] -> 45727
    let digitsToInt32 digits =
        digits
        |> Seq.rev
        |> Seq.mapi (fun i x -> (pown 10 i) * x )
        |> Seq.sum

    /// Takes a seq of 1 or 0 digits and converts them into a single number
    /// based on position. e.g. [ 1; 0; 1; 1; 0 ] -> 22
    let binaryDigitsToInt32 digits =
        digits
        |> Seq.rev
        |> Seq.mapi (fun i x -> (int32 (pown 2 i)) * x )
        |> Seq.sum

let memoize f =
    let dict = Dictionary<_,_>()
    fun x ->
        if dict.ContainsKey x then
            dict.[x]
        else
            let value = f x
            dict.Add(x, value)
            value

let memoize2 f =
    let dict = Dictionary<_,_>()
    fun a b ->
        if dict.ContainsKey (a,b) then
            dict.[(a,b)]
        else
            let value = f a b
            dict.Add((a,b), value)
            value

