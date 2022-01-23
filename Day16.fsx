#load "./Helper.fsx"
open Helper

// Day 16 - BITS
//
// Part 1 - Decoding a recursive datastructure packed into a hex string
// Part 2 - Running operations based on packet types
//
// Remarks: Used a raw byte array for the type, with a bunch of bit shifts for
//          extracting the data for a given bit and length

let byteSlice (packet : byte[]) pos length =
    let rec byteSlice' (packet : byte[]) pos length (carry : int64) =
        let startingByte = pos / 8
        let bitPos = pos % 8
        if length <= (8 - bitPos) then
            let byteSlice = packet.[startingByte] <<< bitPos >>> (8 - length)
            carry ||| int64 byteSlice
        else
            let readLength = (8 - bitPos)
            let byteSlice = packet.[startingByte] <<< bitPos >>> (8 - readLength)
            let remainingLength = length - readLength
            let carry = carry ||| ((int64 byteSlice) <<< remainingLength)
            byteSlice' packet (pos + readLength) remainingLength carry
    byteSlice' packet pos length 0L

type OperatorType = | Sum | Prod | Max | Min | Gt | Lt | Eq
type PacketType = | Literal | Operator of OperatorType
type LengthType = | TotalBits | NumberOfSubPackets

let getPacketType = function
    // Packets with type ID 4 represent a literal value. Literal value packets encode a single binary number. To do this, the binary number is padded with leading zeroes until its length is a multiple of four bits, and then it is broken into groups of four bits. Each group is prefixed by a 1 bit except the last group, which is prefixed by a 0 bit.
    | 4L -> Literal
    // Packets with type ID 0 are sum packets - their value is the sum of the values of their sub-packets. If they only have a single sub-packet, their value is the value of the sub-packet.
    | 0L -> Operator Sum
    // Packets with type ID 1 are product packets - their value is the result of multiplying together the values of their sub-packets. If they only have a single sub-packet, their value is the value of the sub-packet.
    | 1L -> Operator Prod
    // Packets with type ID 2 are minimum packets - their value is the minimum of the values of their sub-packets.
    | 2L -> Operator Min
    // Packets with type ID 3 are maximum packets - their value is the maximum of the values of their sub-packets.
    | 3L -> Operator Max
    // Packets with type ID 5 are greater than packets - their value is 1 if the value of the first sub-packet is greater than the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.
    | 5L -> Operator Gt
    // Packets with type ID 6 are less than packets - their value is 1 if the value of the first sub-packet is less than the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.
    | 6L -> Operator Lt
    // Packets with type ID 7 are equal to packets - their value is 1 if the value of the first sub-packet is equal to the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.
    | 7L -> Operator Eq
    | x -> failwithf "No operator for packet type: %d" x

let getLengthType = function
    //If the length type ID is 0, then the next 15 bits are a number that represents the total length in bits of the sub-packets contained by this packet.
    | 0L -> TotalBits
    //If the length type ID is 1, then the next 11 bits are a number that represents the number of sub-packets immediately contained by this packet.
    | _ -> NumberOfSubPackets

let [<Literal>] VersionLength = 3
let [<Literal>] TypeIdLength = 3
let [<Literal>] LengthTypeIdLength = 1
let [<Literal>] TotalBitsCountLength = 15
let [<Literal>] SubPacketCountLength = 11

let readLiteralPartOfPacket pos (packet : byte[]) =
    let rec readLiteralFromPacket' pos (packet : byte[]) acc =
        let cont = byteSlice packet pos 1
        let chunk = byteSlice packet (pos + 1) 4
        let acc = (acc <<< 4) ||| chunk
        match cont with
        | 1L -> readLiteralFromPacket' (pos + 5) packet acc
        | _ -> (pos + 5), acc
    readLiteralFromPacket' pos packet 0L

let rec readPacket pos (packet : byte[]) =

    let rec readOperatorPacketWithBitsTotal pos
                                           bitsTotal
                                           (packet : byte[])
                                           (versionAcc, valuesAcc) =
        if bitsTotal > 0 then
            let newPos, versionOut, valOut = readPacket pos packet
            let lengthRead = newPos - pos
            readOperatorPacketWithBitsTotal
                newPos (bitsTotal - lengthRead) packet (versionAcc + versionOut, valOut::valuesAcc)
        else
            (pos, versionAcc, valuesAcc |> List.rev)

    let rec readOperatorPacketWithNumberOfSubPackets pos
                                                   numberOfSubPackets
                                                   (packet : byte[])
                                                   (versionAcc, valuesAcc) =
        if numberOfSubPackets > 0 then
            let newPos, versionOut, valOut = readPacket pos packet
            readOperatorPacketWithNumberOfSubPackets
                newPos (numberOfSubPackets - 1) packet (versionAcc + versionOut, valOut::valuesAcc)
        else
            (pos, versionAcc, valuesAcc |> List.rev)

    let packetVersion = byteSlice packet pos VersionLength
    let pos = pos + VersionLength
    let packetTypeId = byteSlice packet pos TypeIdLength
    let packetType = getPacketType packetTypeId
    let pos = pos + TypeIdLength

    match packetType with
    | Literal ->
        let newPos, value = readLiteralPartOfPacket pos packet
        (newPos, packetVersion, value)

    | Operator op ->
        let packetLengthTypeId = byteSlice packet pos LengthTypeIdLength
        let lengthType = getLengthType packetLengthTypeId
        let pos = pos + LengthTypeIdLength

        let newPos, versionTotal, subValues =
            match lengthType with
            | TotalBits ->
                //If the length type ID is 0, then the next 15 bits are a number that represents the total length in bits of the sub-packets contained by this packet.
                let totalBitsCount = byteSlice packet pos TotalBitsCountLength
                let pos = pos + TotalBitsCountLength
                readOperatorPacketWithBitsTotal pos (int totalBitsCount) packet (0L, [])

            | NumberOfSubPackets ->
                //If the length type ID is 1, then the next 11 bits are a number that represents the number of sub-packets immediately contained by this packet.
                let subPacketCount = byteSlice packet pos SubPacketCountLength
                let pos = pos + SubPacketCountLength
                readOperatorPacketWithNumberOfSubPackets pos (int subPacketCount) packet (0L, [])

        let result =
            match op with
            | Sum -> List.sum subValues
            | Prod -> List.reduce (*) subValues
            | Max -> List.max subValues
            | Min -> List.min subValues
            | Gt -> if subValues.[0] > subValues.[1] then 1L else 0L
            | Lt -> if subValues.[0] < subValues.[1] then 1L else 0L
            | Eq -> if subValues.[0] = subValues.[1] then 1L else 0L

        (newPos, versionTotal + packetVersion, result)

let input =
    readLinesWithHashComments "day16.txt"
    |> Seq.map System.Convert.FromHexString
    |> Seq.toList

let part1 packets =
    packets
    |> List.map (readPacket 0)
    //|> List.map (tee (printfn "%A"))
    |> List.head
    |> (fun (_, versionTotal, _) -> versionTotal)
    // Correct Answer: 854 took: 4ms

let part2 packets =
    packets
    |> List.map (readPacket 0)
    //|> List.map (tee (printfn "%A"))
    |> List.head
    |> (fun (_, _, calcResult) -> calcResult)
    // Correct Answer: 186189840660took: 0ms


Helper.measurePart1 part1 input
Helper.measurePart2 part2 input
