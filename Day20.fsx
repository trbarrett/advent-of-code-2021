#load "./Helper.fsx"
open Helper

// Day 20 - Trench Map (Binary, 2d map, game of life)
//
// Remarks:
//   I used a Map to represent the image, which lets us have an infinite
//   potential size for the image.
//   Rather than building up the image layer by layer from the input, I started
//   with the output layer and worked downwards. As we don't know what's in the
//   next layer, we have to inspect the layer below and so on. We do this until
//   we hit the lowest layer which is the input image, then work our way back
//   up. This method let's us inspect a potentially infinite canvas.
//   Note: At each layer we cache the result of each pixel as we find it so we
//   don't need to recalculate everything.
//
// Note: Having an infinite size means we could potentially have to transform
// infinite pixels. In fact for the real problem in the algo we have 1 in the 0
// space (and 0 in the 511 space), so an infinite amount of pixels flip flop for
// each cycle of the image. We need to account for that in our calculation, but
// obviously not perform infinite calculations. So, rather than calculate the
// whole thing on each step, we should only calculate what we need, when we need
// it.
// Both part 1 and part 2 have even number of steps. If they had an odd number
// the answer would be infinite in size.

let readChar = function | '#' -> 1 | '.' -> 0 | _ -> failwith "Invalid input char"

let rec parseImage lines width lineNo existingImage =
   match lines with
   | [] 
   | ""::_ -> existingImage, width, lineNo
   | line::xs ->
       let imageLine =
           line |> Seq.mapi (fun i ch -> (i, lineNo), readChar ch) |> Map
       let width = imageLine |> Seq.length
       let newImage = Map.merge (fun x y -> Option.orElse x y |> Option.get) existingImage imageLine
       parseImage xs width (lineNo + 1) newImage

let parseInput lines =
    let firstLine::_::rest = lines |> Seq.toList
    let algorithm = firstLine |> Seq.map readChar |> Seq.toArray
    let inputImage, width, height = parseImage rest 0 0 Map.empty
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

let rec getNthDegreeImagePixel degree (algo : int32 []) (imgLayers : Map<(int * int), int> list) (pos : int * int) =
    // check the cache first
    match Map.tryFind pos (List.head imgLayers) with
    | Some value -> imgLayers, value
    | None ->
        match imgLayers with
        | [] -> failwith "Should never have 0 layers"
        | inputImg::[] -> [Map.add pos 0 inputImg], 0
        | topLayer::lowerLayers ->
            let x, y = pos
            // get the surrounding values and find the value from the algo image
            // Let's unroll it to make it a bit more obvious.
            // Note we update the cache each time.
            let lowerLayers, topLeft = getNthDegreeImagePixel (degree-1) algo lowerLayers (x-1, y-1)
            let lowerLayers, topMiddle = getNthDegreeImagePixel (degree-1) algo lowerLayers (x, y-1)
            let lowerLayers, topRight = getNthDegreeImagePixel (degree-1) algo lowerLayers (x+1, y-1)
            let lowerLayers, middleLeft = getNthDegreeImagePixel (degree-1) algo lowerLayers (x-1, y)
            let lowerLayers, middleMiddle = getNthDegreeImagePixel (degree-1) algo lowerLayers (x, y)
            let lowerLayers, middleRight = getNthDegreeImagePixel (degree-1) algo lowerLayers (x+1, y)
            let lowerLayers, bottomLeft = getNthDegreeImagePixel (degree-1) algo lowerLayers (x-1, y+1)
            let lowerLayers, bottomMiddle = getNthDegreeImagePixel (degree-1) algo lowerLayers (x, y+1)
            let lowerLayers, bottomRight = getNthDegreeImagePixel (degree-1) algo lowerLayers (x+1, y+1)
            
            let value =
                [ topLeft; topMiddle; topRight
                  middleLeft; middleMiddle; middleRight
                  bottomLeft; bottomMiddle; bottomRight ]
                |> Math.binaryDigitsToInt32

            let pixel = algo.[value]
            (Map.add pos pixel topLayer)::lowerLayers, pixel

let getNthDegreeOutputImage degree algo inputImage width height =
    // the size of the output image will increase by 1 each time, (if we ignore
    // the infinite flip-flopping)
    let positions = 
        [ for x in (0-degree)..(width + degree) do
            for y in (0-degree)..(height + degree) do
                (x, y) ]

    let addEmptyLayer prevLayers = Map.empty::prevLayers
    let buildLayers =
        if degree = 0 then id
        else List.replicate degree addEmptyLayer |> List.reduce (>>)
    let layers = buildLayers [ inputImage ]

    let outputLayers = 
        (layers, positions)
        ||> List.fold (fun layers pos ->
            getNthDegreeImagePixel degree algo layers pos |> fst)

    outputLayers |> List.head


let countLitPixels image =
    Map.values image |> Seq.filter ((=) 1) |> Seq.length

let input = readLinesWithSlashComments "day20.txt" |> parseInput

let part1 (algo, inputImage, width, height) =
    let image = getNthDegreeOutputImage 2 algo inputImage width height
    //printImage image
    countLitPixels image
    // Correct Answer: 4917 took: 233ms

let part2 (algo, inputImage, width, height) =
    let image = getNthDegreeOutputImage 50 algo inputImage width height
    //printImage image
    countLitPixels image
    // Correct Answer: 16,389 took: 35,333ms

Helper.measurePart1 part1 input
Helper.measurePart2 part2 input

