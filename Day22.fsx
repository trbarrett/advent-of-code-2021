#load "./Helper.fsx"
open Helper

// Day 22: Reactor Reboot
//
// The problem is to do with finding the space covered by a set of overlapping
// 3d boxes (cuboids). The hard part is that each subsequent box can be
// on (union) or off (subtraction) from the total space we measure.
// The 3d space is discrite, only using integers, though that doesn't really
// make the problem any more easy.
//
//
// Approach:
//   A naive approach is to turn on and off each point one-by-one for each step,
//   but that is too inefficient. Instead we just consider operations between
//   whole cuboids then, at the end, figure out the space covered by the cuboids.
//
//   We do a very basic type of csg (Constructive Solid Geometry) operation
//   where we subtract the space taken up by a 3d cuboid, from another 3d cuboid
//   and find the remaining shape after. If the remaining shape isn't a cuboid,
//   we break it up into multiple cuboid shapes.
//
//   We work backwards through the steps, because we know the last step to take
//   affect has priority. Working backwards means once we know what has happened
//   in a given space we don't need to consider it again. That way the only
//   operation we need is a subtraction; to figure out what space the new step
//   will cover. If we worked forwards we'd need to do it a bit differently with
//   both a subtraction for "off" cuboids and a union for "on" cuboids since
//   each point could flip/flop.
//
// Performance:
//   While a lot better than the naive approach, this still isn't as fast as I'd
//   like it to be. Once solution would be to implement a BSP (Binary Space
//   Partition) or oct-tree data-structure to do fast lookups of the cuboids
//   within the 3d space. At the moment for each cuboid we add we iterate
//   through every existing one looking for a potential overlap. My assumption
//   is that is the largest performance issue.


// Note:
// Inclusive sizes of Cuboids
// The input X=10..10, Y=10..10, Z=10..10 is considers to be a 1x1 cube where
// each item in the range points to a point in the grid.
// That means the maths in this solution is sometimes a bit odd if you're used
// to dealing with floating point ranges, because 10..10 would normally have a
// size of 0, instead of 1 in this case.

type CubePos = int * int * int
type DimExtents = int * int
type Cuboid = DimExtents * DimExtents * DimExtents
type OnOff = | On | Off

type RangeRelation =
    | Equal
    | MutuallyExclusive
    | BSubsetOfA
    | ASubsetOfB
    | Intersection

module DimExtents =
    let relation (a1, a2) (b1, b2) =
        if a1 = b1 && a2 = b2
        then Equal
        elif b1 > a2 || a1 > b2
        then MutuallyExclusive
        elif a1 <= b1 && a2 >= b2
        then BSubsetOfA
        elif b1 <= a1 && b2 >= a2
        then ASubsetOfB
        else Intersection

type SubtractIntermediateResult =
    | NoIntersection
    | BSubsumesA
    | CuboidsIntersect

module Cuboid =

    let simplifySubtract axisRelations =
        if axisRelations |> List.exists (fun x -> x = MutuallyExclusive) then
            // if just one of the ranges is MutuallyExclusive, then cuboids will
            // never intersect
            NoIntersection
        elif axisRelations|> List.forall (fun x -> x = Equal || x = ASubsetOfB) then
            // if B is a subset of A in all the ranges, then B completely subsumes A
            BSubsumesA
        else
            // otherwise they intersect
            CuboidsIntersect

    let cutEnds (b1, b2) (a1, a2) relation =
        match relation with
        | Equal | ASubsetOfB -> [], (a1, a2) // cut to either side
        | MutuallyExclusive -> [(a1, a2)], (a1, a1)
        | BSubsetOfA ->
            // We may have take both sides or just one
            if a1 = b1 then [(b2+1, a2)], (b1, b2) // only need the right side
            elif b2 = a2 then [(a1, b1-1)], (b1, b2) // only need the left side
            else [(a1, b1-1); (b2+1, a2)], (b1, b2) // need both sides
        | Intersection ->
            if a1 < b1
            then [(a1,b1-1)], (b1, a2) // left cut
            else [(b2+1,a2)], (a1, b2) // right cut

    let cutXRange (bx1, bx2) ((ax1, ax2), (ay1, ay2), (az1, az2)) relation =
        let map1DToCuboid (x1, x2) = (x1, x2), (ay1, ay2), (az1, az2)
        let cuts, remains = cutEnds (bx1, bx2) (ax1, ax2) relation
        cuts |> List.map map1DToCuboid, remains |> map1DToCuboid

    let cutYRange (by1, by2) ((ax1, ax2), (ay1, ay2), (az1, az2)) relation =
        let map1DToCuboid (y1, y2) = (ax1, ax2), (y1, y2), (az1, az2)
        let cuts, remains = cutEnds (by1, by2) (ay1, ay2) relation
        cuts |> List.map map1DToCuboid, remains |> map1DToCuboid

    let cutZRange (bz1, bz2) ((ax1, ax2), (ay1, ay2), (az1, az2)) relation =
        let map1DToCuboid (z1, z2) = (ax1, ax2), (ay1, ay2), (z1, z2)
        let cuts, remains = cutEnds (bz1, bz2) (az1, az2) relation
        cuts |> List.map map1DToCuboid, remains |> map1DToCuboid

    let subtract ((ax1, ax2), (ay1, ay2), (az1, az2) as a)
                 ((bx1, bx2), (by1, by2), (bz1, bz2)) =
        let xRelation = DimExtents.relation (ax1, ax2) (bx1, bx2)
        let yRelation = DimExtents.relation (ay1, ay2) (by1, by2)
        let zRelation = DimExtents.relation (az1, az2) (bz1, bz2)

        match simplifySubtract [xRelation; yRelation; zRelation] with
        | NoIntersection ->
            // there is no intersection between cuboids, so cuboid A remains
            // as it is after the subtraction
            [ a ]
        | BSubsumesA ->
            // The cuboid A is wholly consumed by cuboid B, there is nothing
            // left after the subtraction
            []
        | CuboidsIntersect ->
            // the cuboid A will be partially removed by cuboid B. We need to
            // figure out what remains, and how to represent that as a series of
            // cuboids

            // We cut the cuboid apart go axis by axis
            let newCuboidsX, remaining = cutXRange (bx1, bx2) a xRelation
            let newCuboidsY, remaining = cutYRange (by1, by2) remaining yRelation
            let newCuboidsZ, _ = cutZRange (bz1, bz2) remaining zRelation
            // `remaining` should now be some subset of B, that we don't care about

            // flatten the result into a single list
            List.collect id [newCuboidsX; newCuboidsY; newCuboidsZ]

    let size ((ax1, ax2), (ay1, ay2), (az1, az2)) =
        // note the +1s because it's inclusive. We're measuring whole units
        int64 (ax2 + 1 - ax1)
        * int64 (ay2 + 1 - ay1)
        * int64 (az2 + 1 - az1)
        |> abs

type Step =
    { Switch : OnOff
      Cuboid : Cuboid }

module Step =
    let subtract stepA stepB =
        let newCuboids = Cuboid.subtract stepA.Cuboid stepB.Cuboid
        newCuboids
        |> List.map (fun cuboid -> { Switch = stepA.Switch; Cuboid = cuboid })

let parseLine line : Step =
    let [onoff; x1; x2; y1; y2; z1; z2] =
        String.capture "^(on|off) x=(\-?\d+)..(\-?\d+),y=(\-?\d+)..(\-?\d+),z=(\-?\d+)..(\-?\d+)$" line
    let onoff = match onoff with | "on" -> On | "off" -> Off
    let xExtents = int x1, int x2
    let yExtents = int y1, int y2
    let zExtents = int z1, int z2
    { Switch = onoff
      Cuboid = xExtents, yExtents, zExtents }

let parseInput lines =
    lines |> Seq.map parseLine |> Seq.toList

// We take a list of steps which can override each other, and convert it to a
// list of exclusive steps where there are no cuboids in common, and can't
// override each other.
// The input needs to be list of steps in reverse.
// We work backwards through the list because the later ones always have
// precedence. This way we don't need to add cuboids, or care about switching
// between on/off.
let rec buildExclusiveSteps steps exclusiveSteps =
    match steps with
    | [] -> exclusiveSteps
    | newStep::rest ->
        match exclusiveSteps with
        | [] ->
            // the first step will be empty, we need to set the steps for this
            // to work
            buildExclusiveSteps rest [newStep]
        | _ ->
            // we need to remove any parts of the newStep that intersect with
            // our existing steps, leaving us with on valid new steps.
            let exclusiveNewStepParts =
                ([newStep], exclusiveSteps)
                ||> List.fold (fun newSteps existingStep ->
                    // we can collect here, because we know that each item in
                    // newSteps is mutually exclusive
                    newSteps |> List.collect (fun newStep ->
                        Step.subtract newStep existingStep)
                    )
            buildExclusiveSteps rest (exclusiveNewStepParts@exclusiveSteps)

let constrictSizeForPart1 (step : Step) =
    let (x1, x2), (y1, y2), (z1, z2) = step.Cuboid
    if x1 > 50 || x2 < -50 || y1 > 50 || y2 < -50 || z1 > 50 || z2 < -50
    then None
    else
        Some { step with
                Cuboid = (max x1 -50, min x2 50),
                         (max y1 -50, min y2 50),
                         (max z1 -50, min z2 50) }

let countOnCubes (steps : Step list) =
    steps
    |> List.filter (fun s -> s.Switch = On)
    |> List.sumBy (fun s -> Cuboid.size s.Cuboid)

let input = readLinesWithSlashComments "day22.txt" |> parseInput

let part1 (steps : Step list) =
    let steps = steps |> List.rev
    let steps = steps |> List.choose constrictSizeForPart1
    let exclusiveSteps = buildExclusiveSteps steps []

    countOnCubes exclusiveSteps
    // Correct Answer: 580012 took: 37ms

let part2 (steps : Step list) =
    let steps = steps |> List.rev
    let exclusiveSteps = buildExclusiveSteps steps []
    printfn "TotalSteps: %d" (List.length exclusiveSteps)

    countOnCubes exclusiveSteps
    // Correct Answer: 1334238660555542 took: 15,946ms

Helper.measurePart1 part1 input
Helper.measurePart2 part2 input


