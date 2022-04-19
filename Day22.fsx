#load "./Helper.fsx"
open Helper
open System.Collections.Generic

// Day 22: Reactor Reboot

type CubePos = int * int * int
type DimExtents = int * int
type Cuboid = DimExtents * DimExtents * DimExtents
type OnOff = | On | Off
type Instruction = OnOff * Cuboid

let parseLine line : Instruction =
    let [onoff; x1; x2; y1; y2; z1; z2] =
        String.capture "^(on|off) x=(\-?\d+)..(\-?\d+),y=(\-?\d+)..(\-?\d+),z=(\-?\d+)..(\-?\d+)$" line
    let onoff = match onoff with | "on" -> On | "off" -> Off
    let xExtents = int x1, int x2
    let yExtents = int y1, int y2
    let zExtents = int z1, int z2
    (onoff, (xExtents, yExtents, zExtents))

let parseInput lines =
    lines |> Seq.map parseLine |> Seq.toList

let getCubes ((x0, x1), (y0, y1), (z0, z1)) =
    [ for x in x0..x1 do
        if x >= -50 && x <= 50 then
            for y in y0..y1 do
                if y >= -50 && y <= 50 then
                    for z in z0..z1 do
                        if z >= -50 && z <= 50 then (x, y, z) ]

let updateCubes (state : Dictionary<CubePos, OnOff>) (onOff, cuboid) =
    (state, getCubes cuboid)
    ||> List.fold (fun state pos ->
        if state.ContainsKey(pos)
        then state.[pos] <- onOff
        else state.Add(pos, onOff)
        state)

let countOnCubes (state : Dictionary<CubePos, OnOff>) =
    state |> Seq.filter (fun kvp -> kvp.Value = On) |> Seq.length

// insight for part 2: we can work backwards and ignore anything that is a
// subset of a cube we've already processed. If a new cube intersects with a
// pre-existing cube we can break that new cube up into multiple subcubes, and
// continue on. If we get to many cubes in our history to check we could use
// something like a bsp tree for performance and finding the correct cubes for
// intersection

let input = readLinesWithSlashComments "day22.txt" |> parseInput

let part1 (instructions : Instruction list) =
    let finalState =
        (Dictionary<_,_>(), instructions)
        ||> List.fold (fun state instruction ->
            let state = updateCubes state instruction
            state)
    countOnCubes finalState
    // Correct Answer: 580012 took: 8216ms

Helper.measurePart1 part1 input

