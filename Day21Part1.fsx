#load "./Helper.fsx"
open Helper

// Day 21 - Dirac Dice
//
// Part 1 is quite simple. We are just running through a staged game where
// we roll a standard 6 sided dice a number of times until we have a winner.

type Player = | Player1 | Player2

type GameState =
    { RollNo : int
      PlayerTurn : Player
      Player1Pos : int
      Player1Score : int
      Player2Pos : int
      Player2Score : int }

module GameState =
    let isComplete s = s.Player1Score >= 1000 || s.Player2Score >= 1000
    
    let advance dieValue1 dieValue2 dieValue3 s =
        let moveToNextPos pos =
            let nextPos = (pos + dieValue1 + dieValue2 + dieValue3) % 10
            if nextPos = 0 then 10 else nextPos
        
        match s.PlayerTurn with
        | Player1 ->
            let nextPos = moveToNextPos s.Player1Pos
            { s with
                  RollNo = s.RollNo + 3
                  PlayerTurn = Player2
                  Player1Pos = nextPos
                  Player1Score = s.Player1Score + nextPos }
        | Player2 ->
            let nextPos = moveToNextPos s.Player2Pos
            { s with
                  RollNo = s.RollNo + 3
                  PlayerTurn = Player1
                  Player2Pos = nextPos
                  Player2Score = s.Player2Score + nextPos }

type LoadedDieResult =
    { Value : int
      Next : unit -> LoadedDieResult }

let rec loadedDie next () =
    let next = if next > 1000 then 1 else next
    { Value = next; Next = loadedDie (next + 1) }

let playTurn die state =
   if GameState.isComplete state
   then die, state
   else
       let { Value = dieValue1; Next = die } = die ()
       let { Value = dieValue2; Next = die } = die ()
       let { Value = dieValue3; Next = die } = die ()
       die, GameState.advance dieValue1 dieValue2 dieValue3 state

let rec playGame die state =
    let die, newState = playTurn die state
    if newState <> state
    then playGame die newState
    else newState
       
let part1 (player1StartPos, player2StartPos) =
    let startState = 
        { RollNo = 0
          PlayerTurn = Player1
          Player1Pos = player1StartPos
          Player1Score = 0
          Player2Pos = player2StartPos
          Player2Score = 0 }
    let loadedDie = loadedDie 1
    let endState = playGame loadedDie startState
    endState.RollNo * endState.Player2Score
    // Part 1 result: 921585 took: 0ms

//Helper.measurePart1 part1 (4, 8) // example input
Helper.measurePart1 part1 (6, 7)
 