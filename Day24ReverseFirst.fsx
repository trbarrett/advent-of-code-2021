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

type Num =
    | Val of int64
    | UnknownNum

type Range =
    { From : Num
      To   : Num }

//type UnresolvedInstr =

type Constraint =
    | Unknown
    | Range of Range
    | Value of int
    | UnresolvedInstr of

type Constraints =
    { W : Constraint
      X : Constraint
      Y : Constraint
      Z : Constraint
      Inputs : Constraint list }

module Constraints =

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

// we propagate in reverse

let propagateAddInstr a b =
    // the add instruction takes the value in a, and b (or a const) and stores
    // the value in a after adding them.

    // when running the constraint backwards we are taking the value in a then
    // subtracting that from b to get a.

    // If b is a constant it's a simple subtraction.
    // If b is a range, we're subtracting the range
    // But if B is unknown, then we can't determine anything until we know more about B
    // If A is unknown, we also can't determine anything until we know more about A

    // If we run forward we're in a similar state, we will have unknown values for the
    // input the whole way through. So we need a way to keep the unknown state, and things
    // that will affect it.

    // Going backwards this doesn't add any constraint to B or A, it just subtracts from the Range in A

    // I feel like if something goes from unknown to known we'd have to go back
    // and resolve any unknown values

    // make this generic
    match a, b with
    | Value aValue, Value bValue -> Value (aValue - bValue)
    | Value aValue, Range bRange -> Range (aValue - bRange)
    | Range aRange, Value bValue -> Range (aRange - bValue)
    | Range aRange, Range bRange -> Range (aRange - bRange)
    | Unknown, _
    | _ , Unknown
    // this will be an unresolved instruction, containing another unresolved instruction
    // If we resolve a Value to a range or value, we can go back and fill it in.
    // An Unknown can become known on an input
    | _, UnreslovedInstr _
    | UnreslovedInstr _, _ -> UnreslovedInstr (Add a b)

// constraints can go both backwards and forwards.
// mod x y, adds a backwards constraint that x > 0 any y > 1
// div x y, adds a backwards constraint that y <> 0
// mul x 0, adds a forwards constraint that x = 0.
//          it also means that x becomes unknown before this point

// Constraints need to be lists of conditions I think:
// div x y constrains y to be not 0, but it could already have a range constraint
// of -10 to 10 maybe, so they need to combine somehow.


let doConstraint constraints instruction =
    match instruction with
    | Inp varName ->
        { constraints with
            Inputs = (Constraints.var varName constraints)::constraints.Inputs }
    | Add varName varOrConst ->
        propogateAdd (Constraints.var varName constraints)
                     (Constraints.varOrConst varOrConst constraints)
    | Mul of VarName * VarOrConst
    | Div of VarName * VarOrConst
    | Mod of VarName * VarOrConst
    | Eql of VarName * VarOrConst

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


let input = readLinesWithSlashComments "day22.txt" |> parseInput

let part1 (instructions : Instr list) =
    []
    // Correct Answer: 580012 took: 37ms

Helper.measurePart1 part1 input


//(Program authors should be especially cautious; attempting to execute div with b=0 or attempting to execute mod with a<0 or b<=0 will cause the program to crash and might even damage the ALU. These operations are never intended in any serious ALU program.)
// ^ Can we use this for constraint propogation?
//
// Also:
// Then, after MONAD has finished running all of its instructions, it will indicate
// that the model number was valid by leaving a 0 in variable z.
// ^ This will be our main constraint