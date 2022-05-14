#load "./Helper.fsx"
open Helper

// Day 21 - Dirac Dice
// Part 2
//
// The possible states of the game branch 3 ways each time the dice is rolled.
// We want to find out in how many of those branches each player wins. This is
// simplified from the first game in that each die only has 3 sides, and we
// only play until the first player hits a score of 21.
//
// Remarks:
// The number of possible states is too large to try every option, so we have
// to take advantage of the problem space to simplify things.
//
// The first advantage we can take is to consider the 3 rolls that each player
// makes as a single roll with a pre-determined number of permutations.
//
// The second advantage is we can treat each player as independent from the other
// player. It doesn't matter what the other player rolls, or what position they
// are on, the only time the other player affects the game is when considering
// if the game is over. That allows us to calculate the full universe of states
// for a single player for each turn/step of the game. After that we just need
// to look for winning cases and multiply it by the cases where the other player
// hasn't won, and is still playing. Once we do that for each player, and each
// turn/step of the game we'll have our full count of wins for each player.

let nextPos pos diceTotal =
    let nextPos = (pos + diceTotal) % 10
    if nextPos = 0 then 10 else nextPos

let threeThreeSidedDicePermutations =
    [
      //  3: 111 - 1 permutation
      //  3      - 1 permutation
      3, 1L
      //  4: 211 - 3 permutations
      //  4      - 3 permutations
      4, 3L
      //  5: 311 - 3 permutations
      //  5: 221 - 3 permutations
      //  5      - 6 permutations
      5, 6L
      //  6: 321 - 6 permutations
      //  6: 222 - 1 permutation
      //  6      - 7 permutations
      6, 7L
      //  7: 331 - 3 permutations
      //  7: 322 - 3 permutations
      //  7:     - 6 permutations
      7, 6L
      //  8: 332 - 3 permutations
      //  8:     - 3 permutations
      8, 3L
      //  9: 333 - 1 permutation
      //  9:     - 1 permutations
      9, 1L ]
      // combined there are 27 permutations (3^3)

type Pos = int
type Score = int
type Steps = int
type Universes = int64

type ScoreUniverse = ((Score * Pos) * Universes)

// The game advances by one set of rolls at a time. Since the players are
// independent, the same calculation works for both, we don't have to consider
// them independently. This will calculate the state after the next set of
// rolls, with the possible scores, position, and how many universes end up in\
// that score for a given starting position and number of steps
let calculateNextGameState (scoresUniverses : ScoreUniverse list) : ScoreUniverse list =
    scoresUniverses
    // we ignore scores of 21+. The game is already over for those states
    |> List.filter (fun ((score, _), _) -> score < 21)
    |> List.collect (fun ((score, currPos), universes) ->
        threeThreeSidedDicePermutations
        |> List.map (fun (dice, permutations) ->
            let nextPos = nextPos currPos dice
            let nextScore = score + nextPos
            ((nextScore, nextPos), (universes * permutations)))
    )
    |> List.combineKeyValuePairs (+)
    |> List.sortBy fst

let initialGameScoreTable pos =
    [(0, pos), 1L]

let gameStatesByStep startPos =
    List.unfold (fun (stepNo, gameState) ->
        // max of 21 steps if they somehow got 1 for each step
        if stepNo >= 21
        then None
        else
            let gameState = calculateNextGameState gameState
            let result = (stepNo + 1, gameState)
            Some (result, result)
        ) (0, initialGameScoreTable startPos)

let playerWins (player1StartPos : int, player2StartPos : int) =
    let player1Steps = gameStatesByStep player1StartPos
    let player2Steps = gameStatesByStep player2StartPos

    let player1WinsByStep =
        player1Steps
        |> List.map (fun (step, (scoreUniverses : ScoreUniverse list)) ->
            let winUniverses =
                scoreUniverses
                |> List.filter (fun ((score, _), _) -> score >= 21)
                |> List.sumBy snd

            if winUniverses = 0
            then (step, 0L)
            else
                let player2LostUniverses =
                    player2Steps
                    // (step-1), because player 2 hasn't had their next roll yet
                    |> List.find (fun (s,_) -> s = step-1)
                    |> snd
                    // Note: Player 1 will have lost if player 2 got 21+, in the
                    // previous step, so ignore those
                    |> List.filter (fun ((score, _), _) -> score < 21)
                    |> List.sumBy snd

                // the total universes are not just the number of ways player1 wins,
                // but the number where it wins * the different states player 2 could be
                // in when it loses
                step, winUniverses * player2LostUniverses)

    let player2WinsByStep =
        player2Steps
        |> List.map (fun (step, (scoreUniverses : ScoreUniverse list)) ->
            let winUniverses =
                scoreUniverses
                |> List.filter (fun ((score, _), _) -> score >= 21)
                |> List.sumBy snd

            if winUniverses = 0
            then (step, 0L)
            else
                let player1LostUniverses =
                    player1Steps
                    |> List.find (fun (s,_) -> s = step)
                    |> snd
                    // Note: Player 2 will lose if player 1 also gets 21+
                    // since player 1 always goes first. So ignore those cases
                    |> List.filter (fun ((score, _), _) -> score < 21)
                    |> List.sumBy snd

                // the total universes are not just the number of ways player1
                // wins, but the number where it wins times the different states
                // player 2 could be in when it loses
                step, winUniverses * player1LostUniverses)

    player1WinsByStep |> List.sumBy snd,
    player2WinsByStep |> List.sumBy snd

let part2 (player1StartPos, player2StartPos) =
    playerWins (player1StartPos, player2StartPos)
    // Part 2 result: (911090395997650, 645891163888987) took: 9ms


//Helper.measurePart2 part2 (4, 8) // example input
Helper.measurePart2 part2 (6, 7)