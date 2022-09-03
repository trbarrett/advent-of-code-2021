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


module Num =
    let add a b = a + b

    let mul a b = a * b

    let div a b =
        match a, b with
        | _, 0L -> failwith "Divide by 0"
        | x, 1L -> x
        | a, b -> a / b

    let modulo a b =
        match a, b with
        | _, x when x < 1L -> failwith "Can't modulo when b is less than 1"
        | x, _  when x < 0L -> failwith "Can't modulo when a less than 0"
        | 0L, _ -> 0L // 0 mod anything is 0
        | x, y -> x % y

    let gt a b = a > b

    // TODO **** forward propagation is too slow without Ranges,
    // addition and multiplication just explodes the space of possibilities,
    // especially when combining multiple OneOfs, Ranges might simplify that quite
    // a bit ****
type Constraint =
    | Value of int64
    | OneOf of Constraint List // TODO: This should be a Set perhaps!

type Constraints =
    { W : Constraint
      X : Constraint
      Y : Constraint
      Z : Constraint }

module Constraints =

    let init =
        { W = Value 0L
          X = Value 0L
          Y = Value 0L
          Z = Value 0L }

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

        // simplify where there are multiple values that are the same
        lst |> List.distinct

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
    | Value a, OneOf lst ->
        lst
        |> List.map (propagateAddInstr (Value a))
        |> OneOf
        |> Constraints.simplify
    | OneOf lst, Value b ->
        lst
        |> List.map (propagateAddInstr (Value b))
        |> OneOf
        |> Constraints.simplify
    | OneOf lstA, OneOf lstB ->
        List.allPairs lstA lstB
        |> List.map (fun (a, b) -> propagateAddInstr a b)
        |> OneOf
        |> Constraints.simplify

let rec propagateMulInstr a b =
    match a, b with
    | Value 0L, _
    | _, Value 0L -> Value 0L
    | Value aValue, Value bValue -> Value (aValue * bValue)
    | Value a, OneOf lst ->
        lst
        |> List.map (propagateMulInstr (Value a))
        |> OneOf
        |> Constraints.simplify
    | OneOf lst, Value b ->
        lst
        |> List.map (propagateMulInstr (Value b))
        |> OneOf
        |> Constraints.simplify
    | OneOf lstA, OneOf lstB ->
        List.allPairs lstA lstB
        |> List.map (fun (a, b) -> propagateMulInstr a b)
        |> OneOf
        |> Constraints.simplify

let rec propagateDivInstr a b =
    match a, b with
    | _, Value 0L -> failwith "Can't divide by 0"
    | Value aValue, Value bValue -> Value (aValue / bValue)
    | Value a, OneOf lst ->
        lst
        |> List.map (fun b -> propagateDivInstr (Value a) b)
        |> OneOf
        |> Constraints.simplify
    | OneOf lst, Value b ->
        lst
        |> List.map (fun a -> propagateDivInstr a (Value b))
        |> OneOf
        |> Constraints.simplify
    | OneOf lstA, OneOf lstB ->
        List.allPairs lstA lstB
        |> List.map (fun (a, b) -> propagateDivInstr a b)
        |> OneOf
        |> Constraints.simplify

let rec propagateModInstr a b =
    match a, b with
    | _, Value x when x < 1 -> failwith "Can't modulo when b is less than 1"
    | Value x, _  when x < 0 -> failwith "Can't modulo when a less than 0"
    | Value 0L, _ -> Value 0L // 0 mod anything is 0
    | Value x, Value y when x < y -> Value 0L
    | Value x, Value y -> Value (x % y)
    | Value a, OneOf lst ->
        lst
        |> List.map (fun b -> propagateModInstr (Value a) b)
        |> OneOf
        |> Constraints.simplify
    | OneOf lst, Value b ->
        lst
        |> List.map (fun a -> propagateModInstr a (Value b))
        |> OneOf
        |> Constraints.simplify
    | OneOf lstA, OneOf lstB ->
        List.allPairs lstA lstB
        |> List.map (fun (a, b) -> propagateModInstr a b)
        |> OneOf
        |> Constraints.simplify

let rec propagateEqlInstr a b =
    match a, b with
    | Value x, Value y when x = y -> Value 1L
    | Value _, Value _ -> Value 0L
    | Value a, OneOf lst ->
        lst
        |> List.map (fun b -> propagateEqlInstr (Value a) b)
        |> OneOf
        |> Constraints.simplify
    | OneOf lst, Value b ->
        lst
        |> List.map (fun a -> propagateEqlInstr a (Value b))
        |> OneOf
        |> Constraints.simplify
    | OneOf lstA, OneOf lstB ->
        List.allPairs lstA lstB
        |> List.map (fun (a, b) -> propagateEqlInstr a b)
        |> OneOf
        |> Constraints.simplify

// I'm not sure propogating down going to work? We can't use things like
// div and mod to restrict previous ranges...

let setVarConstraint varName cnstrnt constraints =
    match varName with
    | VarW -> { constraints with W = cnstrnt }
    | VarX -> { constraints with X = cnstrnt }
    | VarY -> { constraints with Y = cnstrnt }
    | VarZ -> { constraints with Z = cnstrnt }

let possibleInputs = OneOf [ for i in 1..9 -> Value i ]

let propagateForwards constraints instruction =
    match instruction with
    | Inp varName ->
        constraints
        |> setVarConstraint varName possibleInputs
        // Set the varName to Unknown
    | Add (varName, varOrConst) ->
        // Set the varName to the result
        let cnstrt = propagateAddInstr (Constraints.var varName constraints)
                                       (Constraints.varOrConst varOrConst constraints)
        constraints |> setVarConstraint varName cnstrt
    | Mul (varName, varOrConst) ->
        let cnstrt =
            propagateMulInstr (Constraints.var varName constraints)
                              (Constraints.varOrConst varOrConst constraints)
        constraints |> setVarConstraint varName cnstrt
    | Div (varName, varOrConst) ->
        let cnstrt =
            propagateDivInstr (Constraints.var varName constraints)
                              (Constraints.varOrConst varOrConst constraints)
        constraints |> setVarConstraint varName cnstrt
    | Mod (varName, varOrConst) ->
        let cnstrt =
             propagateModInstr (Constraints.var varName constraints)
                               (Constraints.varOrConst varOrConst constraints)
        constraints |> setVarConstraint varName cnstrt
    | Eql (varName, varOrConst) ->
        let cnstrt =
             propagateEqlInstr (Constraints.var varName constraints)
                               (Constraints.varOrConst varOrConst constraints)
        constraints |> setVarConstraint varName cnstrt

let reduceToEquals a b =
    match a, b with
    | Value a, Value b when a = b -> Value a, Value b
    | Value a, Value b -> failwith "No valid equals possibility when both are different values"
    | OneOf lst, Value b -> (lst |> List.find (fun x -> x = Value b)), Value b
    | Value a, OneOf lst -> Value a, (lst |> List.find (fun x -> x = Value a))
    | OneOf lstA, OneOf lstB ->
        let intersection = Set.intersect (Set lstA) (Set lstB) |> List.ofSeq
        OneOf intersection, OneOf intersection

let reduceToNotEquals a b =
    match a, b with
    | Value a, Value b when a <> b -> Value a, Value b
    | Value a, Value b -> failwith "No valid NOT equals possibility when both are different values"
    | OneOf lst, Value b -> OneOf (lst |> List.filter (fun x -> x <> Value b)), Value b
    | Value a, OneOf lst -> Value a, OneOf (lst |> List.filter (fun x -> x <> Value a))
    | OneOf lstA, OneOf lstB ->
        // Unless we explore other universes there's not much we can do
        // if there's multiple of both, then we could always pick a combination
        // that's not equal
        OneOf lstA, OneOf lstB

let propagateBackEqlInstr exp a b =
    // We need to set a and b, such that we get exp from a = b
    match exp with
    | Value 0L -> reduceToEquals a b // we need to find matching as in b
    | Value 1L -> reduceToNotEquals a b
    | OneOf [ Value 0L; Value 1L ] ->
        // we can't possibly get a 0L and a 1L result, but I think that's ok. It
        // just means any input can give us 1 of the expected results.
        a, b
    | _ -> failwith "An equals result should be 0 or 1"

let propagateBackAddInstr exp a b =
    // We need to set a and b, such that we get exp from a + b
    match exp with
    | Value exp ->
        match a, b with
        | Value a, Value b when a + b = exp -> Value a, Value b
        | Value a, Value b -> failwith "a + b cannot equal c"
        | OneOf lst, Value b -> (lst |> List.find (fun (Value x) -> x + b = exp)), Value b
        | Value a, OneOf lst -> Value a, (lst |> List.find (fun (Value x) -> a + x = exp))
        | OneOf lstA, OneOf lstB ->
            // find all combinations where a + b could equal exp
            List.allPairs lstA lstB
            |> List.choose (fun (Value a, Value b) ->
                if a + b = exp
                then Some (Value a, Value b)
                else None)
            |> List.unzip
            |> fun (lstA, lstB) ->
                OneOf (List.distinct lstA), OneOf (List.distinct lstB)

    | OneOf oneOfLst ->
        match a, b with
        | Value _, Value _ -> failwithf "Adding two values can't equal a range of values"
        | OneOf lst, Value b ->
            // the result should be equal to oneOfLst
            lst |> List.filter (fun (Value x) -> oneOfLst |> List.contains (Value (x + b))) |> OneOf,
            (Value b)
        | Value a, OneOf lst ->
            (Value a),
            lst |> List.filter (fun (Value x) -> oneOfLst |> List.contains (Value (a + x))) |> OneOf
        | OneOf lstA, OneOf lstB ->
            // we could split into different universes of possibilities for each
            // valid combination. For now lets just keep them all together and
            // see if it works out
            let allViable =
                List.allPairs lstA lstB
                |> List.filter (fun (Value a, Value b) ->
                    oneOfLst |> List.contains (Value (a + b)))
            let a, b = allViable |> List.unzip
            (OneOf a) |> Constraints.simplify, (OneOf b) |> Constraints.simplify

let propagateBackMulInstr exp a b =
    // We need to set a and b, such that we get exp from a * b
    match exp with
    | Value exp ->
        match a, b with
        | Value a, Value b when a * b = exp -> Value a, Value b
        | Value a, Value b -> failwith "a * b cannot equal c"
        | OneOf lst, Value b -> (lst |> List.filter (fun (Value x) -> x * b = exp) |> OneOf), Value b
        | Value a, OneOf lst -> Value a, (lst |> List.filter (fun (Value x) -> a * x = exp) |> OneOf)
        | OneOf lstA, OneOf lstB ->
            // there could be multiple universes that we could get 0.
            // if exp = 0, then either a could be 0, or b could be zero

            // find all combinations where a * b could equal exp
            List.allPairs lstA lstB
            |> List.choose (fun (Value a, Value b) ->
                if a * b = exp
                then Some (Value a, Value b)
                else None)
            |> List.unzip
            |> fun (lstA, lstB) ->
                OneOf (List.distinct lstA), OneOf (List.distinct lstB)

    | OneOf oneOfLst ->
        match a, b with
        | Value _, Value _ -> failwithf "Multiplying two values can't equal a range of values"
        | OneOf lst, Value b ->
            // the result should be equal to oneOfLst
            lst |> List.filter (fun (Value x) -> oneOfLst |> List.contains (Value (x * b))) |> OneOf,
            (Value b)
        | Value a, OneOf lst ->
            (Value a),
            lst |> List.filter (fun (Value x) -> oneOfLst |> List.contains (Value (a * x))) |> OneOf
        | OneOf lstA, OneOf lstB ->
            // we could split into different universes of possibilities for each
            // valid combination. For now lets just keep them all together and
            // see if it works out
            let allViable =
                List.allPairs lstA lstB
                |> List.filter (fun (Value a, Value b) ->
                    oneOfLst |> List.contains (Value (a * b)))
            let a, b = allViable |> List.unzip
            (OneOf a) |> Constraints.simplify, (OneOf b) |> Constraints.simplify

let propagateBackDivInstr exp a b =
    // We need to set a and b, such that we get exp from a / b
    match exp with
    | Value exp ->
        match a, b with
        | Value a, Value b when b <> 0 && a / b = exp -> Value a, Value b
        | Value a, Value b -> failwith "a / b cannot equal c"
        | OneOf lst, Value b -> (lst |> List.filter (fun (Value x) -> b <> 0 && x / b = exp) |> OneOf), Value b
        | Value a, OneOf lst -> Value a, (lst |> List.filter (fun (Value x) -> x <> 0 && a / x = exp) |> OneOf)
        | OneOf lstA, OneOf lstB ->
            // find all combinations where a / b could equal exp
            List.allPairs lstA lstB
            |> List.choose (fun (Value a, Value b) ->
                if b <> 0 && a / b = exp
                then Some (Value a, Value b)
                else None)
            |> List.unzip
            |> fun (lstA, lstB) ->
                OneOf (List.distinct lstA), OneOf (List.distinct lstB)

    | OneOf oneOfLst ->
        match a, b with
        | Value _, Value _ -> failwithf "Dividing two values can't equal a range of values"
        | OneOf lst, Value b ->
            // the result should be equal to oneOfLst
            lst |> List.filter (fun (Value x) -> oneOfLst |> List.contains (Value (x / b))) |> OneOf,
            (Value b)
        | Value a, OneOf lst ->
            (Value a),
            lst |> List.filter (fun (Value x) -> oneOfLst |> List.contains (Value (a / x))) |> OneOf
        | OneOf lstA, OneOf lstB ->
            // we could split into different universes of possibilities for each
            // valid combination. For now lets just keep them all together and
            // see if it works out
            let allViable =
                List.allPairs lstA lstB
                |> List.filter (fun (Value a, Value b) ->
                    oneOfLst |> List.contains (Value (a / b)))
            let a, b = allViable |> List.unzip
            (OneOf a) |> Constraints.simplify, (OneOf b) |> Constraints.simplify

let propagateBackModInstr exp a b =
    // We need to set a and b, such that we get exp from a % b
    match exp with
    | Value exp ->
        match a, b with
        | Value a, Value b when a % b = exp -> Value a, Value b
        | Value a, Value b -> failwith "a % b cannot equal c"
        | OneOf lst, Value b -> (lst |> List.filter (fun (Value x) -> x % b = exp) |> OneOf), Value b
        | Value a, OneOf lst -> Value a, (lst |> List.filter (fun (Value x) -> a % x = exp) |> OneOf)
        | OneOf lstA, OneOf lstB ->
            // find all combinations where a % b could equal exp
            List.allPairs lstA lstB
            |> List.choose (fun (Value a, Value b) ->
                if a % b = exp
                then Some (Value a, Value b)
                else None)
            |> List.unzip
            |> fun (lstA, lstB) ->
                OneOf (List.distinct lstA), OneOf (List.distinct lstB)

    | OneOf oneOfLst ->
        match a, b with
        | Value _, Value _ -> failwithf "Modding two values can't equal a range of values"
        | OneOf lst, Value b ->
            // the result should be equal to oneOfLst
            lst |> List.filter (fun (Value x) -> oneOfLst |> List.contains (Value (x % b))) |> OneOf,
            (Value b)
        | Value a, OneOf lst ->
            (Value a),
            lst |> List.filter (fun (Value x) -> oneOfLst |> List.contains (Value (a % x))) |> OneOf
        | OneOf lstA, OneOf lstB ->
            // we could split into different universes of possibilities for each
            // valid combination. For now lets just keep them all together and
            // see if it works out
            let allViable =
                List.allPairs lstA lstB
                |> List.filter (fun (Value a, Value b) ->
                    oneOfLst |> List.contains (Value (a % b)))
            let a, b = allViable |> List.unzip
            (OneOf a) |> Constraints.simplify, (OneOf b) |> Constraints.simplify



let printConstraint lineNo cnstrnt =
    printfn "LineNo: %A" lineNo
    printfn "W: %A" cnstrnt.W
    printfn "X: %A" cnstrnt.X
    printfn "Y: %A" cnstrnt.Y
    printfn "Z: %A" cnstrnt.Z


let printBackwardsStep lineNo (instruction, constraints) expect =
    printfn ""
    printfn " -- Backwards LineNo: %A --" lineNo
    printfn "Exst W: %A" constraints.W
    printfn "Exst X: %A" constraints.X
    printfn "Exst Y: %A" constraints.Y
    printfn "Exst Z: %A" constraints.Z
    printfn ""
    printfn "Instruction %A" instruction
    printfn ""
    printfn "Expect W: %A" expect.W
    printfn "Expect X: %A" expect.X
    printfn "Expect Y: %A" expect.Y
    printfn "Expect Z: %A" expect.Z

let printBackwardsStepNext next =
    printfn ""
    printfn "Next W: %A" next.W
    printfn "Next X: %A" next.X
    printfn "Next Y: %A" next.Y
    printfn "Next Z: %A" next.Z

let performBackInstr instructionFn constraints expectCnstrnt varName varOrConst =
    let a, (b : Constraint) =
        instructionFn (Constraints.var varName expectCnstrnt)
                      (Constraints.var varName constraints)
                      (Constraints.varOrConst varOrConst constraints)
    let a = Constraints.simplify a

    // For a we replace it
    let constraints = expectCnstrnt |> setVarConstraint varName a
    match varOrConst with
    | Var varName ->
        // But for b we're not replacing the constraint we worked out previously,
        // we're simply reducing the options further
        let bExp = Constraints.var varName constraints
        let b =
            match b, bExp with
            | Value b, Value bExp when b = bExp -> Value b
            | Value _, Value _ -> failwith "Constraint value is incompatible with previous value"
            | OneOf bList, Value bExp
                when bList |> List.contains (Value bExp) -> Value bExp
            | OneOf _, Value _ -> failwith "Constraint list is incompatible with previous value"
            | Value b, OneOf bExpList
                when bExpList |> List.contains (Value b) -> Value b
            | Value _, OneOf _ -> failwith "Constraint list is incompatible with previous value"
            | OneOf bLst, OneOf bExpList ->
                Set.intersect (Set bLst) (Set bExpList)
                |> List.ofSeq |> OneOf
        let b = Constraints.simplify b

        setVarConstraint varName b constraints
    | Const _ -> constraints


let rec propagateBackwardsStep history expectCnstrnt acc =
    match history with
    | _ when List.length history < 0 -> acc
    | [] -> acc
    | (instruction, constraints)::history ->
        printBackwardsStep
            (List.length history + 1) (instruction, constraints) expectCnstrnt

        match instruction with
        | Inp varName ->
           let vals = Constraints.var varName expectCnstrnt
           printfn "***** PossibleValues: %A" vals
           propagateBackwardsStep history expectCnstrnt (vals::acc)
        | Add (varName, varOrConst) ->
            let nextCstrnt =
                performBackInstr
                    propagateBackAddInstr
                    constraints expectCnstrnt varName varOrConst
            printBackwardsStepNext nextCstrnt
            propagateBackwardsStep history nextCstrnt acc
        | Mul (varName, varOrConst) ->
            let nextCstrnt =
                performBackInstr
                    propagateBackMulInstr
                    constraints expectCnstrnt varName varOrConst
            printBackwardsStepNext nextCstrnt
            propagateBackwardsStep history nextCstrnt acc
        | Div (varName, varOrConst) ->
            let nextCstrnt =
                performBackInstr
                    propagateBackDivInstr
                    constraints expectCnstrnt varName varOrConst
            printBackwardsStepNext nextCstrnt
            propagateBackwardsStep history nextCstrnt acc
        | Mod (varName, varOrConst) ->
            let nextCstrnt =
                performBackInstr
                    propagateBackModInstr
                    constraints expectCnstrnt varName varOrConst
            printBackwardsStepNext nextCstrnt
            propagateBackwardsStep history nextCstrnt acc
        | Eql (varName, varOrConst) ->
            let nextCstrnt =
                performBackInstr
                    propagateBackEqlInstr
                    constraints expectCnstrnt varName varOrConst
            printBackwardsStepNext nextCstrnt
            propagateBackwardsStep history nextCstrnt acc

// when propagating backwards we are constraining some values based on
// the calculation if we have a + b = 0, and we work out a has to be 0 and
// b has to be 0, then we need to carry that forwards.
//
// My confusion is we're changing the expected value, and the input value at the
// same time if we change the previous constraint. And that isn't right, but
// if we don't carry the change to b forwards, we're just ignoring the constraint
// that we know about



let propagateBackwards instructions constraints =
    // The model number is valid if Z has a 0, so we start by enforcing that
    // constraint

    // We should do a sanity check to see if last Z contains a zero. If it doesn't
    // then something's gone quite wrong

    let last::constraints = constraints
    let expect = last |> setVarConstraint VarZ (Value 0)
    let lst = List.zip instructions constraints

    propagateBackwardsStep lst expect []

    /// now go through each one backwards


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

let part1 (instructions : Instr list) =
    let instructions = instructions //|> List.take 180

    let forwardConstraints =
        (Constraints.init, instructions)
        ||> List.scan propagateForwards

    forwardConstraints |> List.iteri printConstraint

    propagateBackwards
           (instructions |> List.rev)
           (forwardConstraints |> List.rev)

    //[]

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