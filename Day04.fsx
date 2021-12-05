#load "./Helper.fsx"

open Helper

// Day 4 - Bingo:
//
// Part 1 - Basic bingo game simulator
// Part 2 - Small variation on part 1, where we want to pick the last board to win
//
// Remarks: The solution is neither as succinct, clear or fast as I would like
//          it. On the other hand, it worked first go... so Jackpot?!

type BingoRow = (int * bool) []
type BingoBoard = BingoRow []

module BingoBoard =
    let isWinner (board : BingoBoard) =
        let wonWithRows = board |> Array.exists (Array.forall snd)
        let wonWithColumns =
            [| 0 .. (board.[0].Length - 1) |]
            |> Array.exists (fun column ->
                board |> Array.forall (fun row -> snd row.[column]))
        wonWithRows || wonWithColumns

    let findWinner (boards : BingoBoard []) =
        Array.tryFind isWinner boards

    let markNumber n (board : BingoBoard) : BingoBoard  =
        board |> Array.map (fun row ->
            row |> Array.map (fun (x,isMarked) ->
                if x = n then (x, true) else (x,isMarked)))

let parseBingoBoard input =
    let rec parseBingoBoard' input board =
        match input with
        | [] -> board, []
        | ""::rest -> board, rest
        | xs::rest ->
            xs
            |> String.splitIntoMatching "\d+"
            |> Seq.map (fun x -> int x, false)
            |> Array.ofSeq
            |> fun row -> Array.append board [| row |]
            |> parseBingoBoard' rest

    parseBingoBoard' input [||]

let parseBingoBoards input : BingoBoard [] =
    let rec parseBingoBoards' input (boards : BingoBoard []) =
        match input with
        | [] -> boards
        | _ -> let (board, rest) = parseBingoBoard input
               parseBingoBoards' rest (Array.append boards [| board |])
    parseBingoBoards' input [||]

let parseBingoInput input =
    let bingoNumbers::rest = input // first line is the bingo numbers
    let bingoNumbers = bingoNumbers |> String.split ',' |> Seq.map int |> List.ofSeq
    let _::rest = rest // second line is blank
    let bingoBoards = parseBingoBoards rest // followed by the bingo boards
    bingoNumbers, bingoBoards

let calculateOutputNumber board bingoNumber =
    let totalUnmarkedNumbers =
        board
        |> Array.collect (Array.filter (snd >> not))
        |> Array.map fst |> Array.sum
    totalUnmarkedNumbers * bingoNumber

let input =
    readLinesWithHashComments "day04.txt" |> List.ofSeq |> parseBingoInput

let part1 (bingoNumbers, boards) =
    let rec playNextRound lastNumber bingoNumbers boards =
        match Array.tryFind BingoBoard.isWinner boards with
        | Some winner -> winner, lastNumber
        | None ->
            let n::remainingBingoNumbers = bingoNumbers
            boards
            |> Array.map (BingoBoard.markNumber n)
            |> playNextRound n remainingBingoNumbers

    let winner, lastNumber = playNextRound 0 bingoNumbers boards
    calculateOutputNumber winner lastNumber
    // Correct Answer: 39984 took: 8ms


let part2 (bingoNumbers, boards) =
    let rec playNextRound lastNumber bingoNumbers boards =
        match Array.filter BingoBoard.isWinner boards, boards with
        | winners, [| _ |] -> // we found a winner on the last board
            winners.[0], lastNumber
        | winners, boards ->
            let boards = // remove the winners if we had any
                match winners with
                | [||] -> boards
                | winners -> boards |> Array.filter (fun b -> winners |> Array.contains b |> not)
            let n::remainingBingoNumbers = bingoNumbers
            boards
            |> Array.map (BingoBoard.markNumber n)
            |> playNextRound n remainingBingoNumbers

    let winner, lastNumber = playNextRound 0 bingoNumbers boards
    calculateOutputNumber winner lastNumber
    // Correct Answer: 8468 took: 10ms

Helper.measurePart1 part1 input
Helper.measurePart2 part2 input
