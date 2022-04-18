#load "./Helper.fsx"
open Helper
open System.Collections.Generic

// Day 20 - Trench Map
// 
// Same solution as the immutable version, but using a mutable dictionary rather
// than a Map. This improves the performance massively

let readChar = function | '#' -> 1 | '.' -> 0 | _ -> failwith "Invalid input char"

let rec parseImage lines width lineNo (existingImage : Dictionary<_,_>) =
    match lines with
    | [] 
    | ""::_ -> existingImage, width, lineNo
    | line::xs ->
        let d = Dictionary<_,_>()
        line |> Seq.iteri (fun i ch -> existingImage.Add((i, lineNo), readChar ch) )
        let width = if width = 0 then existingImage.Count else width
        parseImage xs width (lineNo + 1) existingImage

let parseInput lines =
    let firstLine::_::rest = lines |> Seq.toList
    let algorithm = firstLine |> Seq.map readChar |> Seq.toArray
    let inputImage, width, height = parseImage rest 0 0 (Dictionary<_,_>())
    algorithm, inputImage, width, height

let getExtents image =
    let startExtent = (System.Int32.MaxValue, System.Int32.MinValue,
                       System.Int32.MaxValue, System.Int32.MinValue)
    (startExtent, image)
    ||> Map.fold (fun (minX, maxX, minY, maxY) (x,y) _ ->
        (if x < minX then x else minX), (if x > maxX then x else maxX),
        (if y < minY then y else minY), (if y > maxY then y else maxY))

let printImage image =
    let minX, maxX, minY, maxY = getExtents image
    let paper =
        [ for y in minY..maxY ->
            [for x in minX..maxX ->
                match Map.find (x,y) image with
                | 0 -> "." | 1 -> "#"]]
    printfn "\n%s\n" (Seq.toString "\n" (paper |> List.map (Seq.toString "")))

let rec getNthDegreeImagePixel degree (algo : int32 []) (imgLayers : Dictionary<(int * int), int> list) (pos : int * int) =
    // check the cache first
    let topLayer = List.head imgLayers
    match topLayer.GetValueOrDefault(pos, -1) with
    | value when (value = 0 || value = 1) -> value
    | -1 ->
        match imgLayers with
        | [] -> failwith "Should never have 0 layers"
        | inputImg::[] ->
            inputImg.Add(pos, 0)
            0
        | topLayer::lowerLayers ->
            let x, y = pos
            // get the surrounding values and find the value from the algo image
            // Let's unroll it to make it a bit more obvious.
            let topLeft = getNthDegreeImagePixel (degree-1) algo lowerLayers (x-1, y-1)
            let topMiddle = getNthDegreeImagePixel (degree-1) algo lowerLayers (x, y-1)
            let topRight = getNthDegreeImagePixel (degree-1) algo lowerLayers (x+1, y-1)
            let middleLeft = getNthDegreeImagePixel (degree-1) algo lowerLayers (x-1, y)
            let middleMiddle = getNthDegreeImagePixel (degree-1) algo lowerLayers (x, y)
            let middleRight = getNthDegreeImagePixel (degree-1) algo lowerLayers (x+1, y)
            let bottomLeft = getNthDegreeImagePixel (degree-1) algo lowerLayers (x-1, y+1)
            let bottomMiddle = getNthDegreeImagePixel (degree-1) algo lowerLayers (x, y+1)
            let bottomRight = getNthDegreeImagePixel (degree-1) algo lowerLayers (x+1, y+1)
            
            let value =
                [ topLeft; topMiddle; topRight
                  middleLeft; middleMiddle; middleRight
                  bottomLeft; bottomMiddle; bottomRight ]
                |> Math.binaryDigitsToInt32
            
            let pixel = algo.[value]
            topLayer.Add(pos, pixel)
            pixel

let getNthDegreeOutputImage degree algo inputImage width height =
    // the size of the output image will increase by 1 each time, (if we ignore
    // the infinite flip-flopping)
    
    let addEmptyLayer prevLayers = (new Dictionary<_,_>())::prevLayers
    let buildLayers =
        if degree = 0 then id
        else List.replicate degree addEmptyLayer |> List.reduce (>>)
    let layers = buildLayers [ inputImage ]
    
    [ for x in (0-degree)..(width + degree) do
        for y in (0-degree)..(height + degree) do
            getNthDegreeImagePixel degree algo layers (x, y) ] |> ignore
    
    layers |> List.head

let countLitPixels (image : Dictionary<_,_>) =
    image.Values |> Seq.filter ((=) 1) |> Seq.length

let input = readLinesWithSlashComments "day20.txt" |> parseInput

let part1 (algo, inputImage, width, height) =
    let image = getNthDegreeOutputImage 2 algo inputImage width height
    //printImage image
    countLitPixels image
    // Correct Answer: 4917 took: 73ms

let part2 (algo, inputImage, width, height) =
    let image = getNthDegreeOutputImage 50 algo inputImage width height
    //printImage image
    countLitPixels image
    // Correct Answer: 16,389 took: 5,990ms (Much, much, faster with a mutable dictionary)

Helper.measurePart1 part1 input
Helper.measurePart2 part2 input

