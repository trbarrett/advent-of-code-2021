// Day 24: Arithmetic Logic Unit
//
// The problem gives us a simple program written in a very simple arithmetic
// language, with only 4 variables. At 14 points the program takes an input
// and we have to figure out what is the maximum value of each input (1-9) that
// would leave a 0 in the Z variable at the end
//
// Approach:
// This long program is really a single subroutine repeated 14 times. There are
// 3 things that change in each repetition, and by using those changing
// parameters we can figure out what each input could be.
//
// One important observation is that the X and Y variables don't carry between
// each iteration of the subroutine, only the Z variable does. Looking at the
// Z variable we can see that it base 26, as in each subroutine we either
// divide or multiple by 26 (shifting left or right).
//
// At the end of each subroutine we either shift left and add a positive number
// or shift right. Given there are an equal amount of left and right shifts, and
// we have to end with Z=0 we can put a restriction on what paths of logic the
// algorithm must follow.
//
// From there we can work out the results by hand with very simple arithemtic



// The algorithm utilizes a base 26 Z number
// * we divide by 26 (shift right)
// * we multiply by 26 (shift left)
// * we get the remainder of (z % 26) (least significant digit)

// The algorithm works on 14 cycles of a 18 line subroutine.
// There are only 3 values that change:
// line 5: div z 26 or div z 1
// line 6: add x ?
// line 16: add y ?

// based on the algorithm I've called them: ShiftRight, WComparison and WPlus
// * ShiftRight indicates if we should shift the number right one place (z / 26)
//   (based   on a base 26 number)
// * WComparison is a number we add to the least significant digit in z (z % 26)
//   and compare it vs W
// * WPlus is a number we add to W and place on the end of z after it has been
//   shifted left (z = z * 26)

type Modifiers =
    { ShiftRight : bool
      WComparison : int64
      WPlus : int64 }

let stages =
    [ { ShiftRight = false; WComparison =  10; WPlus =  2 }
      { ShiftRight = false; WComparison =  10; WPlus =  4 }
      { ShiftRight = false; WComparison =  14; WPlus =  8 }
      { ShiftRight = false; WComparison =  11; WPlus =  7 }
      { ShiftRight = false; WComparison =  14; WPlus = 12 }
      { ShiftRight =  true; WComparison = -14; WPlus =  7 }
      { ShiftRight =  true; WComparison =   0; WPlus = 10 }
      { ShiftRight = false; WComparison =  10; WPlus = 14 }
      { ShiftRight =  true; WComparison = -10; WPlus =  2 }
      { ShiftRight = false; WComparison =  13; WPlus =  6 }
      { ShiftRight =  true; WComparison = -12; WPlus =  8 }
      { ShiftRight =  true; WComparison =  -3; WPlus = 11 }
      { ShiftRight =  true; WComparison = -11; WPlus =  5 }
      { ShiftRight =  true; WComparison =  -2; WPlus = 11 } ]

// base 26 Z represented by a list of ints. Starts from the least signifigant
type Base26 = private | Base26 of int list
module Base26 =
    let init = Base26 [ 0 ]
    let rightMost (Base26 item) = // same as z % 26
        match item with
        | [] -> 0
        | x::xs -> x
    let shiftLeft (Base26 item) = Base26 (0::item) // same as z * 26
    let shiftLeftAndAdd (Base26 item) toAdd = Base26 (toAdd::item) // same as z * 26
    let shiftRight (Base26 item) = // same as z / 26
        match item with
        | [] -> Base26 []
        | x::xs -> Base26 xs

// this is the 18 line algorithm simplified to it's raw exentials

let step w (z : Base26) shiftRight wComparison wPlus =
    let z = if shiftRight
            then Base26.shiftRight z
            else z
    let z =
        if w = Base26.rightMost z + wComparison
        then z
        else Base26.shiftLeftAndAdd z (w + wPlus)
    z

// Some analysis:

// If wComparision is > 8 then it will never match,
// since w is [1..9]
//
// w = z % 26 + WComparision can only be true for w = 9 when z = 0, and z has
// to be 1 at a minimum

// For ShiftRight = false, all the wComparison numbers are > 9
// so we will always take the wPlus path
// For ShiftRight = true, all the wComparison numbers are <= 0

// Note: wPlus is always > 0

// For us to get to Z = 0 at the end, we need to remove all the items we push
// onto Z. So our constraint is that we have to force the algo to go down the
// (w = z % 26 + wcomparision) path whenever ShiftRight = true.

// Knowing that, we can build a set of constraints between the numbers. Then
// using the fact we need to get the highest number we can figure out what they
// are:

(*
 working forwards
 W 0 - no shift
 z = [w0+2]
 W 1 - no shift
 z = [w0+2][w1+4]
 W 2 - no shift
 z = [w0+2][w1+4][w2+8]
 W 3 - no shift
 z = [w0+2][w1+4][w2+8][w3+7]
 W 4 - no shift
 z = [w0+2][w1+4][w2+8][w3+7][w4+12]
 W 5 - SHIFT - comparision: -14
 when: w5 = w4 + 12 - 14 OR w5 = w4 - 2
 z = [w0+2][w1+4][w2+8][w3+7]
 W 6 - SHIFT - comparision: 0
 when: w6 = w3 + 7 - 0 OR w6 = w3 + 7
 z = [w0+2][w1+4][w2+8]
 W 7 - no shift
 z = [w0+2][w1+4][w2+8][w7+14]
 W 8 - SHIFT - comparision: -10
 when: w8 = w7 + 14 - 10 OR w8 = w7 + 4
 z = [w0+2][w1+4][w2+8]
 W 9 - no shift
 z = [w0+2][w1+4][w2+8][w9+6]
 W 10 - SHIFT - comparision: -12
 when: w10 = w9 + 6 - 12 OR w10 = w9 -6
 z = [w0+2][w1+4][w2+8]
 W 11 - SHIFT - comparision: -3
 when: w11 = w2 + 8 - 3 OR w11 = w2 + 5
 z = [w0+2][w1+4]
 W 12 - SHIFT - comparision: -11
 when: w12 = w1 + 4 - 11 OR w12 = w1 -7
 z = [w0+2]
 W 13 - SHIFT - comparision: -2
 when: w13 = w0 + 2 - 2 OR w13 = w0
 z = 0

 Largest possible

 w0 = w13        // w0 = 9, w13 = 9
 w1 = w12 + 7    // w1 = 9, w12 = 2
 w2 = w11 - 5    // w2 = 4, w11 = 9
 w9 = w10 + 6    // w9 = 9, w10 = 3
 w8 = w7 + 4     // w8 = 9, w7 = 5
 w6 = w2 + 7     // w6 = 9, w3 = 2
 w5 = w4 - 2     // w5 = 7, w4 = 9

 99429795993929


// smallest possible

w0 = w13        // w0 = 1, w13 = 1
w1 = w12 + 7    // w1 = 8, w12 = 1
w2 = w11 - 5    // w2 = 1, w11 = 6
w9 = w10 + 6    // w9 = 7, w10 = 1
w8 = w7 + 4     // w8 = 5, w7 = 1
w6 = w2 + 7     // w6 = 8, w3 = 1
w5 = w4 - 2     // w5 = 1, w4 = 3

18113181571611

*)
