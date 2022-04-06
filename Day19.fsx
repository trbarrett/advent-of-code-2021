#load "./Helper.fsx"
open Helper

// Day 19 - Beacon Scanner
//
// Building a 3d map, from multiple samples (beacons) at unknown points
// (scanners) and orientations.
// * The beacons in each scanner cube have positions relative to the scanner
//   itself.
// * Each scanner cube overlaps with one or more others. The problem definition
//   tells us that at least 12 beacons will be common between them when they
//   match.
// * Each scanner cube can be rotated by 90 degrees any number of times, in any
//   axis (X, Y, Z).
// * Our objective is to convert all the relative positions from individual
//   scanners into positions relative to scanner 0. 
//
// Approach:
// (1) Within each scanner cube, for each beacon, determine the distance to each
//     other beacon (squared)
// (2) Add the first scanner to a list of scanners we know the correct positions
//     of.
// (3) Starting from the scanners with correct positions, go through each
//     unmatched scanner and compare the beacons with those in the the found
//     scanners. If a subset of the distances match (11) then we've found a
//     likely candidate.
// (4) For the matching distances, find the largest one, and rotate the
//     comparison scanner cube until it matches. Then rotate all the beacons in
//     that match and see if 11 of them match. (If no match check the next scanner)
// (5) If we did get a match, convert the matching scanner cube to positions
//     relative to the start scanner. (If no match check the next scanner)
// (6) Add the matching scanner to a list of scanners we know the final positions
//     of and go back to 3. If there are no remaining scanners then we're done.

type ScannerCube =
    { ScannerNo : int
      ScannerPos : int * int * int
      Beacons : Set<int * int * int>  }

module ScannerCube =
    let centreOnBeacon (x, y, z) cube =
        let translatePos (x', y', z') = x' - x, y' - y, z' - z
        { ScannerNo = cube.ScannerNo
          ScannerPos = translatePos cube.ScannerPos
          Beacons = cube.Beacons |> Set.map translatePos }
        , (-x, -y, -z)

    let getOrderedBeacons cube =
        cube.Beacons |> Set.toList |> List.sortDescending

    let printScanner cube =
        let scannerStr = sprintf "--- scanner (%d) --- \n" cube.ScannerNo
        let (x, y, z) = cube.ScannerPos
        let scannerPosStr = sprintf " = scannerpos (%5d, %5d, %5d) = \n" x y z
        let beaconsStr =
            getOrderedBeacons cube
            |> List.map (fun (x,y,z) -> sprintf "%5d, %5d, %5d" x y z)
            |> Seq.toString "\n"
        scannerStr + scannerPosStr + beaconsStr

let parseScanner lines =
    let title::lines = lines
    let [scannerNo] = String.capture "--- scanner (\d+) ---" title
    let beacons =
        lines
        |> List.map (fun str ->
            let [x; y; z] = String.capture "(-?\d+),(-?\d+),(-?\d+)" str
            (int x, int y, int z))
        |> Set
    { ScannerNo = int scannerNo; ScannerPos = 0, 0, 0; Beacons = beacons }

type BeaconDistances =
    { ScannerNo : int
      BeaconOfConcern : (int * int * int)
      SquaredDistances: Set<int64> }

type ScannerWithDistances =
    { ScannerCube : ScannerCube
      BeaconsWithDistances : BeaconDistances list }

// For the Beacons in cube A and cube B, try to find a pair where the distance
// to the surrounding beacons match. If there is a match, there will 12 or more
// matching pairs, but we just need the first one
let findMatchingCubesDistances (scannerABeacons : ScannerWithDistances)
                               (scannerBBeacons : ScannerWithDistances) =
    scannerABeacons.BeaconsWithDistances
    // tryPick, because we'll stop when we find the first match
    |> List.tryPick (fun beaconADistances -> 
        scannerBBeacons.BeaconsWithDistances
        |> List.tryPick (fun beaconBDistances ->
            let intersect = Set.intersect beaconADistances.SquaredDistances
                                          beaconBDistances.SquaredDistances
            // 11 instead of 12, because the beacon we're measuring from counts as 1
            if Set.count intersect >= 11 
            then Some (beaconADistances.BeaconOfConcern, beaconBDistances.BeaconOfConcern)
            else None))

type Rotation =
    | NoRotation
    | X90Degrees | X180Degrees | X270Degrees
    | Y90Degrees | Y180Degrees | Y270Degrees
    | Z90Degrees | Z180Degrees | Z270Degrees

// A list of ways we can get all 24 different cube orientations via axis rotations
//
// I find it simple to think about putting one face of the cube on the ground,
// then trying each 4 rotations with the face on the ground. Then do that 6
// times, 1 for each face. You can use vector math to do this faster, but for
// this problem just do dumb rotations to start with.
let cubeOrientations =
    [[NoRotation];  [Y90Degrees];              [Y180Degrees];              [Y270Degrees]
     [Z90Degrees];  [Z90Degrees; Y90Degrees];  [Z90Degrees; Y180Degrees];  [Z90Degrees; Y270Degrees]
     [Z180Degrees]; [Z180Degrees; Y90Degrees]; [Z180Degrees; Y180Degrees]; [Z180Degrees; Y270Degrees]
     [Z270Degrees]; [Z270Degrees; Y90Degrees]; [Z270Degrees; Y180Degrees]; [Z270Degrees; Y270Degrees]
     [X90Degrees];  [X90Degrees; Y90Degrees];  [X90Degrees; Y180Degrees];  [X90Degrees; Y270Degrees]
     [X270Degrees]; [X270Degrees; Y90Degrees]; [X270Degrees; Y180Degrees]; [X270Degrees; Y270Degrees]]

let performRotation (x, y, z) rotation =
     match rotation with
     | NoRotation -> (x, y, z)
     | X90Degrees -> (x, -z, y) | X180Degrees -> (x, -y, -z) | X270Degrees -> (x, z, -y)
     | Y90Degrees -> (-z, y, x) | Y180Degrees -> (-x, y, -z) | Y270Degrees -> (z, y, -x)
     | Z90Degrees -> (y, -x, z) | Z180Degrees -> (-x, -y, z) | Z270Degrees -> (-y, x, z)

let performRotations rotations (x, y, z) =
     ((x, y, z), rotations)
     ||> List.fold performRotation

type Transform =
    | Translation of (int * int * int)
    | Rotation of Rotation list

module Transform =
    let perform t (x, y, z) =
        match t with
        | Translation (dx, dy, dz) -> (x + dx, y + dy, z + dz)
        | Rotation rotations -> performRotations rotations (x, y, z)

    let performSequence transforms (x, y, z) =
        ((x, y, z), transforms)
        ||> List.fold (flip perform)

let transformScannerCube (transforms: Transform list) (cube : ScannerCube) =
    { cube with
        Beacons = cube.Beacons |> Set.map (Transform.performSequence transforms)
        ScannerPos = cube.ScannerPos |> (Transform.performSequence transforms) }

// for two cubes, with two trial points from each cube, we want to try to find
// a rotation that matches the trial point, and see if the whole beacon matches
// when rotated the same way
let findRotationToMatch (scannerA : ScannerCube) (ax, ay, az)
                        (scannerB : ScannerCube) (bx, by, bz) =

    // Two options here: (a) we try all 24 possible orientations, or (b) we do a
    // smarter method looking at the actual data.
    // Let's do (a) all 24 rotations, it's easier if slower
    cubeOrientations
    |> List.tryPick (fun rotations ->
        if Transform.perform (Rotation rotations) (bx, by, bz) = (ax, ay, az)
        then
            // we found a matching trial rotation, now rotate the whole cube and
            // try to match
            let bRotated = transformScannerCube [(Rotation rotations)] scannerB

            // we need to match 12 beacons. (0, 0, 0) will be one of the one's we're matching
            let intersection = Set.intersect scannerA.Beacons bRotated.Beacons
            if Set.count intersection >= 12
            then Some rotations
            else None
        else None)

let neg (x,y,z) = -x,-y,-z

let transformBeaconDistances (transforms : Transform list) (beaconDistances : BeaconDistances) =
    { beaconDistances with
        BeaconOfConcern = Transform.performSequence transforms beaconDistances.BeaconOfConcern }
    
// Find the largest neighbour beacon from cubeA that matches one or more beacons
// in cubeB. Since only 12 need to match, we're not guaranteed that the largest
// neighbour in beaconA will match any in beaconB, so we have to keep trying
// until we find a match, if any.
let findLargestMatchingNeighbours (cubeA : ScannerCube) (cubeB : ScannerCube) =
    ScannerCube.getOrderedBeacons cubeA
    // 0,0,0 will always match, so isn't useful for our purpose
    |> List.filter ((<>) (0,0,0)) 
    |> List.tryPick (fun (x, y, z) ->
        let neighbour = Set [abs x; abs y; abs z]
        let matching =
            cubeB.Beacons
            |> Set.filter (fun (x', y', z') ->
                neighbour = Set [abs x'; abs y'; abs z'])
        match Set.count matching with
        | 0 -> None
        | _ -> Some ((x, y, z), matching))

// We have two scanner cubes, and two beacons from each cube which we think may
// be the same if we can get the right rotation and transform.
let checkMatchAndFindTransform (beaconA : int * int * int)
                               (cubeA : ScannerCube)
                               (beaconB : int * int * int)
                               (cubeB : ScannerCube) =
    // precondition is that 11 neighbours have matching distances between both
    // cubes. We expect the beacons specified in each cube match once we
    // transform and translate them

    // translate A and B cubes to put beaconA and beaconB at the origin (0,0,0)
    let cubeA, translationA = ScannerCube.centreOnBeacon beaconA cubeA
    let cubeB, translationB = ScannerCube.centreOnBeacon beaconB cubeB

    // We have to start with something to compare, so simply pick the largest
    match findLargestMatchingNeighbours cubeA cubeB with
    | None -> None
    | Some ((ax, ay, az), possibleMatches) ->
        // we've found a point in A, and one or more points in B that could
        // match given the correct rotations. We'll try the different rotations
        // of the possible matches
        possibleMatches
        |> Seq.tryPick (fun (bx, by, bz) ->
            match findRotationToMatch cubeA (ax, ay, az) cubeB (bx, by, bz) with
            | None -> None
            | Some rotation ->
                // Create a set of transforms from a point in B to A
                Some [Translation translationB
                      Rotation rotation
                      Translation (neg translationA)])

// Given a "start" scanner cube, and a bunch of others to check this function
// finds the first one that has 12 matching beacons, and normalizes the position
// to the "start" scanner's orientation and position 
let rec findNextAndNormalize (start : ScannerWithDistances)
                             (remaining : ScannerWithDistances list) =
    match remaining with
    | [] -> None
    | x::xs ->
        // First do a quick check to see if there are any matching beacons
        // between the scanner cubes, based purely on the distances to
        // neighbours beacons.
        match findMatchingCubesDistances start x with
        | None -> findNextAndNormalize start xs // no luck, try the next one
        | Some (startBeacon, beaconX) ->
            // we've found a potential match where the distances to neighbours
            // match. Now try to find a matching transformation for all the
            // beacons.
            match checkMatchAndFindTransform startBeacon start.ScannerCube beaconX x.ScannerCube with
            | None -> findNextAndNormalize start xs // no luck, try the next one
            | Some transforms ->
                // We've found a matching bunch of transformations. Let's
                // transform our scanner distance and return it
                Some { x with
                        BeaconsWithDistances =
                            x.BeaconsWithDistances
                            |> List.map (transformBeaconDistances transforms)
                        ScannerCube =
                            transformScannerCube transforms x.ScannerCube }

// This is a breadth first search which attempts to find how each scanner cube
// matches with the others, and normalize them to the same coordinate system
let rec findAndNormalizeAll (found : ScannerWithDistances list)
                            (remaining : ScannerWithDistances list) =
    match remaining with
    | [] -> found
    | _ ->
        // go through each previously matched up scanner cubes and see if we can find a match
        // with any of the beacons in the remaining list
        let nextShared = found |> List.pick (fun x -> findNextAndNormalize x remaining)
        let remaining =
            remaining |> List.filter (fun x ->
                x.ScannerCube.ScannerNo <> nextShared.ScannerCube.ScannerNo)
        findAndNormalizeAll (nextShared::found) remaining
        
let beaconDistances (x, y, z) scanner =
    let distancesSq =
        scanner.Beacons
        |> Set.map (fun (x', y', z') ->
            (pown (int64 (x' - x)) 2) + (pown (int64 (y' - y)) 2) + (pown (int64 (z' - z)) 2))
        |> Set.filter (fun x -> x <> 0L) // filter out the beacon of concern
    { ScannerNo = scanner.ScannerNo
      BeaconOfConcern = (x, y, z)
      SquaredDistances = distancesSq }

let scannerBeaconDistances scanner =
    scanner.Beacons
    |> Set.map (fun beacon -> beaconDistances beacon scanner)
    |> List.ofSeq
    |> List.sortBy (fun x -> x.BeaconOfConcern)
    |> (fun beaconsWithDistances ->
        { ScannerCube = scanner
          BeaconsWithDistances = List.ofSeq beaconsWithDistances } )

let calcAllBeaconDistances scanners =
    scanners |> List.map scannerBeaconDistances

let getCenteredScanners (scanners : ScannerCube list) =
    // get all the distances within each scanner to their neighbour beacons
    let scannerBeacons = scanners |> calcAllBeaconDistances
    // setup the breadth first search, starting with scanner 0
    let start, remaining =
        scannerBeacons |> List.partition (fun x -> x.ScannerCube.ScannerNo = 0)
    // run the breadth first search
    let allFound = findAndNormalizeAll start remaining
    let finalScannerCubes = allFound |> List.map (fun x -> x.ScannerCube)

    finalScannerCubes


let getLargestManhattenDistance (points : Set<int * int * int>) =
    points
    |> Seq.toList
    |> List.combinations 2
    |> List.map (fun [(x,y,z);(x',y',z')] ->
        (abs (x - x')) + (abs (y - y')) + (abs (z - z')), [(x,y,z);(x',y',z')])
    |> List.maxBy fst


let parseInput lines = splitOnEmptyLines lines |> List.map parseScanner

let input = readLinesWithHashComments "day19.txt" |> parseInput

let part1 scanners =
    (getCenteredScanners scanners)
    |> List.map (fun x -> x.Beacons)
    |> Set.unionMany
    |> Set.count
    // Correct Answer: 394 took: 572ms

let part2 scanners =
    (getCenteredScanners scanners)
    |> List.map (fun x -> x.ScannerPos)
    |> Set
    |> getLargestManhattenDistance
    // Correct Answer: 12304 took: 547ms

Helper.measurePart1 part1 input
Helper.measurePart2 part2 input
