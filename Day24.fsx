#load "./Helper.fsx"
open Helper

// Day 24: Arithmetic Logic Unit
(*
Magic smoke starts leaking from the submarine's arithmetic logic unit (ALU).
Without the ability to perform basic arithmetic and logic functions, the
submarine can't produce cool patterns with its Christmas lights!

It also can't navigate. Or run the oxygen system.

Don't worry, though - you probably have enough oxygen left to give you enough
time to build a new ALU.

The ALU is a four-dimensional processing unit: it has integer variables w, x, y,
and z. These variables all start with the value 0. The ALU also supports six
instructions:

    inp a - Read an input value and write it to variable a.
    add a b - Add the value of a to the value of b, then store the result in
              variable a.
    mul a b - Multiply the value of a by the value of b, then store the result
              in variable a.
    div a b - Divide the value of a by the value of b, truncate the result to an
              integer, then store the result in variable a. (Here, "truncate"
              means to round the value toward zero.)
    mod a b - Divide the value of a by the value of b, then store the remainder
              in variable a. (This is also called the modulo operation.)
    eql a b - If the value of a and b are equal, then store the value 1 in
              variable a. Otherwise, store the value 0 in variable a.

In all of these instructions, a and b are placeholders; a will always be the
variable where the result of the operation is stored (one of w, x, y, or z),
while b can be either a variable or a number. Numbers can be positive or
negative, but will always be integers.

The ALU has no jump instructions; in an ALU program, every instruction is run
exactly once in order from top to bottom. The program halts after the last
instruction has finished executing.

(Program authors should be especially cautious; attempting to execute div with
b=0 or attempting to execute mod with a<0 or b<=0 will cause the program to
crash and might even damage the ALU. These operations are never intended in any
serious ALU program.)

For example, here is an ALU program which takes an input number, negates it, and
stores it in x:

inp x
mul x -1

Here is an ALU program which takes two input numbers, then sets z to 1 if the
second input number is three times larger than the first input number, or sets
z to 0 otherwise:

inp z
inp x
mul z 3
eql z x

Here is an ALU program which takes a non-negative integer as input, converts it
into binary, and stores the lowest (1's) bit in z, the second-lowest (2's) bit
in y, the third-lowest (4's) bit in x, and the fourth-lowest (8's) bit in w:

inp w
add z w
mod z 2
div w 2
add y w
mod y 2
div w 2
add x w
mod x 2
div w 2
mod w 2

Once you have built a replacement ALU, you can install it in the submarine,
which will immediately resume what it was doing when the ALU failed: validating
the submarine's model number. To do this, the ALU will run the MOdel Number
Automatic Detector program (MONAD, your puzzle input).

Submarine model numbers are always fourteen-digit numbers consisting only of
digits 1 through 9. The digit 0 cannot appear in a model number.

When MONAD checks a hypothetical fourteen-digit model number, it uses fourteen
separate inp instructions, each expecting a single digit of the model number in
order of most to least significant. (So, to check the model number
13579246899999, you would give 1 to the first inp instruction, 3 to the second
inp instruction, 5 to the third inp instruction, and so on.) This means that
when operating MONAD, each input instruction should only ever be given an
integer value of at least 1 and at most 9.

Then, after MONAD has finished running all of its instructions, it will indicate
that the model number was valid by leaving a 0 in variable z. However, if the
model number was invalid, it will leave some other non-zero value in z.

MONAD imposes additional, mysterious restrictions on model numbers, and legend
says the last copy of the MONAD documentation was eaten by a tanuki. You'll need
to figure out what MONAD does some other way.

To enable as many submarine features as possible, find the largest valid
fourteen-digit model number that contains no 0 digits. What is the largest model
number accepted by MONAD?
*)

type VarName = | VarW | VarX | VarY | VarZ

type VarOrConst = | Var of VarName | Const of int

type Instr =
    | Inp of VarName
    | Add of VarName * VarOrConst
    | Mul of VarName * VarOrConst
    | Div of VarName * VarOrConst
    | Mod of VarName * VarOrConst
    | Eql of VarName * VarOrConst

let parseVarOrConst varOrConstStr =
    match varOrConstStr with
    | "w" -> Var VarW
    | "x" -> Var VarX
    | "y" -> Var VarY
    | "z" -> Var VarZ
    | x -> Const (int x)

let parseVar varStr =
    match varStr with
    | "w" -> VarW | "x" -> VarX | "y" -> VarY | "z" -> VarZ

let parseInput lines =
    [ for line in lines do
        let instrStr::varStr::rest = String.split ' ' line |> List.ofArray
        match instrStr with
        | "inp" -> Inp (parseVar varStr)
        | "add" -> Add ((parseVar varStr), (parseVarOrConst rest.[0]))
        | "mul" -> Mul ((parseVar varStr), (parseVarOrConst rest.[0]))
        | "div" -> Div ((parseVar varStr), (parseVarOrConst rest.[0]))
        | "mod" -> Mod ((parseVar varStr), (parseVarOrConst rest.[0]))
        | "eql" -> Eql ((parseVar varStr), (parseVarOrConst rest.[0])) ]

type UnknownType = | Neg | Pos

module UnknownType =
    let flip = function | Neg -> Pos | Pos -> Neg

// I'm not going to disambigate infinity to start with
type Num =
    | Val of int64
    | Unknown of UnknownType

module Num =
    let add a b =
        match a, b with
        | Val a, Val b -> Val (a + b)
        | Val _, Unknown x
        | Unknown x, Val _ -> Unknown x
        | Unknown x, Unknown y when x = y -> Unknown x
        | Unknown x, Unknown y -> failwith "Cannot add different types of Unknown"

    let mul a b =
        match a, b with
        | Val a, Val b -> Val (a * b)
        | Val x, Unknown y
        | Unknown y, Val x ->
            if x > 0L
            then Unknown y
            else Unknown (UnknownType.flip  y)
        | Unknown x, Unknown y when x = y -> Unknown x
        | Unknown x, Unknown y -> failwith "Cannot multiple different types of Unknown"

    let div a b =
        match a, b with
        | _, Val 0L -> failwith "Divide by 0"
        | x, Val 1L -> x
        | Val a, Val b -> Val (a / b)
        | Unknown x, Val _ -> Unknown x
        // technically n/inf is 0, and inf/inf is 1,
        // but with unknown ranges we have to think about it differently
        | Val a, Unknown x -> Unknown x
        | Unknown x, Unknown y when x = y -> Unknown x
        | Unknown x, Unknown y when x = y -> Val 1L
        | Unknown x, Unknown y -> failwith "Cannot divide different types of Unknown"

    let modulo a b =
        match a, b with
        | _, Val x when x < 1 -> failwith "Can't modulo when b is less than 1"
        | _, Unknown Neg -> failwith "Can't modulo when b is less than 1"
        | Val x, _  when x < 0 -> failwith "Can't modulo when a less than 0"
        | Unknown Neg, _ -> failwith "Can't modulo when a less than 0"
        | Val 0L, _ -> Val 0 // 0 mod anything is 0
        | Val x, Val y when x < y -> Val 0
        | Val x, Val y -> Val (x % y)
        | Unknown Pos, Val _ -> Unknown Pos
        | Val _, Unknown Pos -> Unknown Pos
        | Unknown Pos, Unknown Pos -> Unknown Pos

    let gt a b =
        match a, b with
        | Val a, Val b -> a > b
        | Val _, Unknown Pos -> false
        | Val _, Unknown Neg -> true
        | Unknown Pos, Val _ -> true
        | Unknown Neg, Val _ -> false
        | Unknown Pos, Unknown Pos -> false
        | Unknown Neg, Unknown Neg -> false
        | Unknown Pos, Unknown Neg -> true
        | Unknown Neg, Unknown Pos -> false

type Range =
    { From : Num
      To   : Num }

type Constraint =
    | Range of Range
    | Value of int64
    | OneOf of Constraint List // TODO: This should be a Set perhaps!

module Range =

    let zero = { From = Val 0L; To = Val 0L }

    let intersection rangeA rangeB =
        match rangeA.From, rangeA.To, rangeB.From, rangeB.To with
        | Val aFrom, Val aTo, Val bFrom, Val bTo ->
            let rFrom = max aFrom bFrom
            let rTo = min aTo bTo
            if rFrom > rTo
            then None
            else Some { From = Val rFrom; To = Val rTo }
        | Unknown x, Val aTo, _, Val bTo
        | _, Val aTo, Unknown x, Val bTo ->
            Some { From = Unknown x; To =  Val (min aTo bTo) }
        | Val aFrom, Unknown x, Val bFrom, _
        | Val aFrom, _, Val bFrom, Unknown x ->
            Some { From = Val (max aFrom bFrom); To = Unknown x }
        | Unknown aFrom, Unknown aTo, Unknown bFrom, Unknown bTo
            when aFrom = bFrom && aTo = bTo -> Some rangeA
        | _ -> None

    let union rangeA rangeB =
        match rangeA.From, rangeA.To, rangeB.From, rangeB.To with
        | Val aFrom, Val aTo, Val bFrom, Val bTo
            when aTo > bFrom || bTo > aFrom ->
            let rFrom = max aFrom bFrom
            let rTo = min aTo bTo
            Some { From = Val rFrom; To = Val rTo }
        | Val aFrom, Val aTo, Val bFrom, Val bTo ->
            None // theres a gap between them
        | Unknown x, Val aTo, _, Val bTo
        | _, Val aTo, Unknown x, Val bTo ->
            Some { From = Unknown x; To =  Val (max aTo bTo) }
        | Val aFrom, Unknown x, Val bFrom, _
        | Val aFrom, _, Val bFrom, Unknown x ->
            Some { From = Val (min aFrom bFrom); To = Unknown x }
        | Unknown aFrom, Unknown aTo, Unknown bFrom, Unknown bTo
            when aFrom = bFrom && aTo = bTo -> Some rangeA
        | _ -> None

    let simplifyList ranges =
        let rec unionRanges ranges r =
           match ranges with
           | xr::rs ->
               match union r xr with
               | Some union -> unionRanges rs union
               | None -> xr::unionRanges rs r
           | [] -> []

        let foldRange ranges r =
            if List.length ranges = 0
            then [r]
            else unionRanges ranges r

        ([], ranges) ||> List.fold foldRange


    let ensureOrder range =
        if Num.gt range.From range.To
        then { From = range.To; To = range.From }
        else range

    let addValue a range =
        { From = Num.add (Val a) range.From
          To   = Num.add (Val a) range.To }

    let addRange a b =
        { From = Num.add a.From b.From
          To   = Num.add a.To b.To }

    let mulValue a range =
        if a = 0L then
            zero
        else
            { From = Num.mul (Val a) range.From
              To   = Num.mul (Val a) range.To }
            |> ensureOrder

    let mulRange a b =
        if a = zero || b = zero then
            zero
        else
            { From = Num.mul a.From b.From
              To   = Num.mul a.To b.To }
            |> ensureOrder

    let divRangeByValue range a =
        if a = 0L then
            failwith "Cannot divide by 0"
        elif a = 1L then
            range
        else
            { From = Num.div range.From (Val a)
              To   = Num.div range.To (Val a) }
            |> ensureOrder

    let divValueByRange a range =
        if a = 0L then
            zero
        else
            { From = Num.div (Val a) range.From
              To   = Num.div (Val a) range.To }
            |> ensureOrder

    let divRange a b =
        if a = zero || b = zero then
            zero
        else
            { From = Num.div a.From b.From
              To   = Num.div a.To b.To }
            |> ensureOrder

    let modRangeByValue range a =
        if a < 1L then
            failwith "Cannot mod range by less than 1"
        else
            { From = Num.modulo range.From (Val a)
              To   = Num.modulo range.To (Val a) }
            |> ensureOrder
    let modValueByRange a range =
        if a < 0L then
            failwith "Cannot mod by range when less than 0"
        else
            { From = Num.modulo (Val a) range.From
              To   = Num.modulo (Val a) range.To }
            |> ensureOrder

    let modRange rangeA rangeB =
        { From = Num.modulo rangeA.From rangeB.From
          To   = Num.modulo rangeA.To rangeA.To }
        |> ensureOrder

    let containsValue x range =
        match range.From, range.To with
        | Unknown _, _
        | _, Unknown _ -> None // we can't know, since we don't know what x is
        | Val rFrom, Val rTo when x >= rFrom && x <= rTo -> Some true
        | Val rFrom, Val rTo -> Some false


type Constraints =
    { W : Constraint
      X : Constraint
      Y : Constraint
      Z : Constraint
      Inputs : Constraint list }

module Constraints =

    let init =
        { W = Value 0L
          X = Value 0L
          Y = Value 0L
          Z = Value 0L
          Inputs = [] }

    let var varName constraints =
       match varName with
       | VarW -> constraints.W
       | VarX -> constraints.X
       | VarY -> constraints.Y
       | VarZ -> constraints.Z

    let varOrConst varOrConst constraints =
       match varOrConst with
       | Var x ->
           match x with
           | VarW -> constraints.W
           | VarX -> constraints.X
           | VarY -> constraints.Y
           | VarZ -> constraints.Z
       | Const x -> Value x

    let rec simplify cnstrnt =
        match cnstrnt with
        | Value _ -> cnstrnt
        | Range { From = Val x; To = Val y } when x = y -> Value x
        | Range _ -> cnstrnt
        | OneOf [x] -> x
        | OneOf lst ->
            match (simplifyList lst) with
            | [x] -> x
            //| lst -> OneOf (List.sort lst)
            | lst -> OneOf (List.sort lst)

    and simplifyList lst : Constraint List =
        // flatten one-ofs that are nested
        let lst =
            lst |> List.collect (function
                | OneOf innerLst -> innerLst
                | x -> [x])

        // simplify where there's a range with a single value
        let lst =
            lst |> List.map (function
                | Range { From = Val x; To = Val y } when x = y -> Value x
                | x -> x)

        // simplify where there are multiple values that are the same
        let lst = lst |> List.distinct

        // split out the different types
        let ranges, values, oneOfs =
            lst |> List.partition3WaysBy (function
                | Range x -> Choice1Of3 x
                | Value x -> Choice2Of3 x
                // there shouldn't be any OneOfs left - we flattened them
                | OneOf x -> Choice3Of3 (OneOf x))

        // combine ranges that overlap
        let ranges = Range.simplifyList ranges

        // remove values that are also present in ranges
        let values =
            values |> List.filter (fun v ->
                ranges |> List.exists (fun range ->
                    match Range.containsValue v range with
                    | Some b -> b
                    | None -> false)
                |> not) // include values that are NOT in any range

        [for x in values -> Value x]
        @ [for x in ranges -> Range x]
        @ oneOfs

let rec propagateAddInstr a b =
    // the add instruction takes the value in a, and b (or a const) and stores
    // the value in a after adding them.

    // when running the constraint backwards we are taking the value in a then
    // subtracting that from b to get a.

    // If b is a constant it's a simple subtraction.
    // If b is a range, we're subtracting the range
    // But if B is unknown, then we can't determine anything until we know more about B
    // If A is unknown, we also can't determine anything until we know more about A

    // So we need a way to keep the unknown state, and things that will affect it.
    // Going backwards this doesn't add any constraint to B or A, it just subtracts from the Range in A
    // I feel like if something goes from unknown to known we'd have to go back
    // and resolve any unknown values

    // If we go forward I don't know that we need this. For a Inp instr we can
    // just set an infinite range. It might be easier to try going forwards to
    // start with.

    match a, b with
    | Value aValue, Value bValue -> Value (aValue + bValue)
    | Value aValue, Range bRange -> Range (Range.addValue aValue bRange)
    | Range aRange, Value bValue -> Range (Range.addValue bValue aRange)
    | Range aRange, Range bRange -> Range (Range.addRange aRange bRange)
    | x, OneOf lst
    | OneOf lst, x ->
        lst |> List.map (propagateAddInstr x) |> OneOf |> Constraints.simplify

let rec propagateMulInstr a b =
    match a, b with
    | Value 0L, _
    | _, Value 0L -> Value (0L)
    | Value aValue, Value bValue -> Value (aValue * bValue)
    | Value aValue, Range bRange -> Range (Range.mulValue aValue bRange)
    | Range aRange, Value bValue -> Range (Range.mulValue bValue aRange)
    | Range aRange, Range bRange -> Range (Range.mulRange aRange bRange)
    | x, OneOf lst
    | OneOf lst, x ->
        lst |> List.map (propagateMulInstr x) |> OneOf |> Constraints.simplify

let rec propagateDivInstr a b =
    match a, b with
    | _, Value 0L -> failwith "Can't divide by 0"
    | Value aValue, Value bValue -> Value (aValue / bValue)
    | Value aValue, Range bRange -> Range (Range.divValueByRange aValue bRange)
    | Range aRange, Value bValue -> Range (Range.divRangeByValue aRange bValue)
    | Range aRange, Range bRange -> Range (Range.divRange aRange bRange)
    | x, OneOf lst
    | OneOf lst, x ->
        // try out ingoring invalid options
        let lst = lst |> List.filter (fun x -> match x with | Value x -> x <> 0 | _ -> true)
        lst |> List.map (propagateDivInstr x) |> OneOf |> Constraints.simplify

let rec propagateModInstr a b =
    match a, b with
    | _, Value x when x < 1 -> failwith "Can't modulo when b is less than 1"
    | Value x, _  when x < 0 -> failwith "Can't modulo when a less than 0"
    | Value 0L, _ -> Value 0L // 0 mod anything is 0
    | Value x, Value y -> Value (x % y)
    | Value aValue, Range bRange -> Range (Range.modValueByRange aValue bRange)
    | Range aRange, Value bValue -> Range (Range.modRangeByValue aRange bValue)
    | Range aRange, Range bRange -> Range (Range.modRange aRange bRange)
    | x, OneOf lst ->
        let lst = lst |> List.filter (fun x -> match x with | Value x -> x > 0 | _ -> true)
        lst |> List.map (propagateModInstr x) |> OneOf |> Constraints.simplify
    | OneOf lst, x ->
        let lst = lst |> List.filter (fun x -> match x with | Value x -> x > 1 | _ -> true)
        lst |> List.map (propagateModInstr x) |> OneOf |> Constraints.simplify


// if range cannot contain, then 0, if range matches exactly, then 1
// else, Range of (0, 1)
let rangeEqlValue a range =
    match a, range.From, range.To with
    | Val a, Val b, Val c when a = b && a = c -> Value 1L // range matches exactly, so 1
    | Val a, Val b, Val c when a < b && a > c -> Value 0L // range can never contain the value, so 0
    | _ -> OneOf [ Value 0L; Value 1L ]

let rangeEqlRange rangeA rangeB =
    match rangeA.From, rangeA.To, rangeB.From, rangeB.To with
    // note even if the two ranges have equal extents we don't consider
    // them to be equal. In our model an item could take on any element in
    // the range, and could still be unequal
    | Val aFrom, Val aTo, Val bFrom, Val bTo
        when aFrom = aTo && aFrom = bFrom && aFrom = bTo -> Value 1L
    | Val aFrom, Val aTo, Val bFrom, Val bTo
        when aFrom > bTo || aTo < bFrom -> Value 0L // range can never contain the value, so 0
    | _ -> OneOf [ Value 0L; Value 1L ]

let rec propagateEqlInstr a b =
    match a, b with
    | Value x, Value y when x = y -> Value 1L
    | Value _, Value _ -> Value 0L
    | Value aValue, Range bRange -> rangeEqlValue (Val aValue) bRange
    | Range aRange, Value bValue -> rangeEqlValue (Val bValue) aRange
    | Range aRange, Range bRange -> rangeEqlRange aRange bRange
    | x, OneOf lst
    | OneOf lst, x ->
        lst |> List.map (propagateEqlInstr x) |> OneOf |> Constraints.simplify

// I'm not sure propogating down going to work? We can't use things like
// div and mod to restrict previous ranges...

let setVarConstraint varName cnstrnt constraints =
    match varName with
    | VarW -> { constraints with W = cnstrnt }
    | VarX -> { constraints with X = cnstrnt }
    | VarY -> { constraints with Y = cnstrnt }
    | VarZ -> { constraints with Z = cnstrnt }

let possibleInputs = OneOf [ for i in 1..9 -> Value i ]

let doConstraint constraints instruction =
    match instruction with
    | Inp varName ->
        { constraints with
            Inputs = (Constraints.var varName constraints)::constraints.Inputs }
        |> setVarConstraint varName possibleInputs
        // Set the varName to Unknown
    | Add (varName, varOrConst) ->
        // Set the varName to the result
        constraints
        |> setVarConstraint
             varName
             (propagateAddInstr (Constraints.var varName constraints)
                                (Constraints.varOrConst varOrConst constraints))
    | Mul (varName, varOrConst) ->
        constraints
        |> setVarConstraint
             varName
             (propagateMulInstr (Constraints.var varName constraints)
                                (Constraints.varOrConst varOrConst constraints))
    | Div (varName, varOrConst) ->
        constraints
        |> setVarConstraint
             varName
             (propagateDivInstr (Constraints.var varName constraints)
                                (Constraints.varOrConst varOrConst constraints))
    | Mod (varName, varOrConst) ->
        constraints
        |> setVarConstraint
             varName
             (propagateModInstr (Constraints.var varName constraints)
                                (Constraints.varOrConst varOrConst constraints))
    | Eql (varName, varOrConst) ->
        constraints
        |> setVarConstraint
             varName
             (propagateEqlInstr (Constraints.var varName constraints)
                                (Constraints.varOrConst varOrConst constraints))

// Next steps - We could add an evaluator to try out different inputs,
// but I don't think it will get us to right the solution. It might get us to
// the solution for part 1, but I doubt it will help with part 2

// Otherwise we could jump right in and start to do a constraint solver, though
// it's not obvious what form that would take, so it would be quite exploritory.
// I'm not even sure if it would be better to start from the bottom or the top
// to build the contraints. If we start from the top we'd probably need to do a
// 2 pass thing.
//
// Hmm. Or we could split things by INP instruction, and process each section
// individually

// Constraints could be range of numbers that a value could be.

let printConstraint lineNo cnstrnt =
    printfn "LineNo: %A" lineNo
    printfn "W: %A" cnstrnt.W
    printfn "X: %A" cnstrnt.X
    printfn "Y: %A" cnstrnt.Y
    printfn "Z: %A" cnstrnt.Z

let part1 (instructions : Instr list) =
    let instructions = instructions //|> List.take 219

    (Constraints.init, instructions)
    ||> List.scan doConstraint
    |> List.iteri printConstraint

    []
    // Correct Answer: 580012 took: 37ms

let input = readLinesWithSlashComments "day24.txt" |> parseInput

Helper.measurePart1 part1 input


//(Program authors should be especially cautious; attempting to execute div with b=0 or attempting to execute mod with a<0 or b<=0 will cause the program to crash and might even damage the ALU. These operations are never intended in any serious ALU program.)
// ^ Can we use this for constraint propogation?
//
// Also:
// Then, after MONAD has finished running all of its instructions, it will indicate
// that the model number was valid by leaving a 0 in variable z.
// ^ This will be our main constraint