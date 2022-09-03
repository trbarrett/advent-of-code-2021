#load "./Helper.fsx"
#load "./Range.fsx"
open Helper
open Range

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


    // TODO **** forward propagation is too slow without Ranges,
    // addition and multiplication just explodes the space of possibilities,
    // especially when combining multiple OneOfs, Ranges might simplify that quite
    // a bit ****

type Constraint =
    | Value of int64
    | Range of Range
    | OneOf of Constraint List // TODO: This should be a Set perhaps!

let flattenOneOfList oneOffList =
   oneOffList
   |> List.collect (function
       | Value x -> [Value x]
       | OneOf lst -> lst)

let rec expandRangeConstraints cnstrt =
    match cnstrt with
    | Value x -> Value x
    | Range r -> Range.toValues r |> List.map Value |> OneOf
    | OneOf xs ->
        xs
        |> List.map expandRangeConstraints
        |> flattenOneOfList
        |> OneOf

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

    // does A contain each value in B
    let rec isVarSuperSet cnstrntA cnstrntB =
        printfn "cnstrntA: %A -- cnstrntB: %A" cnstrntA cnstrntB
        match cnstrntA, cnstrntB with
        | Value a, Value b -> a = b
        | Value a, Range b -> { From = a; To = a } = b
        | Range a, Value b -> a |> Range.containsValue b
        | Range a, Range b -> Range.tryIntersect a b = Some b
        | Value a, OneOf b -> [ Value a ] = b
        | OneOf aLst, Value b -> aLst |> List.contains (Value b)
        | OneOf aLst, OneOf bLst ->
            let aSet = Set aLst
            // check that for every item in b, there is a corresponding item in a
            bLst |> List.forall (fun b -> aSet |> Set.contains b)
        | OneOf aLst, Range b -> isVarSuperSet (OneOf aLst) (expandRangeConstraints (Range b))
        | Range a, OneOf bLst -> isVarSuperSet (expandRangeConstraints (Range a)) (OneOf bLst)

    // does A contain each value in B
    let isSuperSet a b =
        isVarSuperSet a.W b.W
        && isVarSuperSet a.X b.X
        && isVarSuperSet a.Y b.Y
        && isVarSuperSet a.Z b.Z

    let rec simplify cnstrnt =
        match cnstrnt with
        | Value _ -> cnstrnt
        | Range { From = a; To = b } when a = b -> Value a
        | Range _ -> cnstrnt
        | OneOf [x] -> x
        | OneOf lst ->
            match (simplifyList lst) with
            | [x] -> x
            //| lst -> OneOf (List.sort lst)
            | lst -> OneOf lst

    // TODO -> Simplify ranges
    and simplifyList lst : Constraint List =
        // flatten one-ofs that are nested
        let lst =
            lst |> List.collect (function
                | OneOf innerLst -> innerLst
                | x -> [x])

        // Major performance boost if we can limit the size of Z. It grows a
        // great deal, but we also shrink it by a great deal by dividing by 26
        // a bunch of times. This number is a guess for what the max should be
        //
        // If we get a bad back propagation result we probably need to mess with
        // this.

        let lst =
            lst
            |> List.filter (function
                | Value x when x > 20_000_000L -> false
                | Range { From = x } when x > 20_000_000L -> false
                | _ -> true)

        // simplify where there are multiple values that are the same
        let lst =
            lst
            |> List.distinct
            |> List.sortBy (function
                | Value x -> x
                | Range { To = x } -> x
                | OneOf _ -> failwith "Should have remove nested list")

        // * Convert 2 adjacent values into a range
        // * Combine an adjacent value with a range
        // * Combine 2 ranges that are adjacent
        // * Combine 2 ranges that intersect
        (lst, [])
        ||> List.foldBack (fun x acc  ->
            match acc with
            | [] -> [ x ]
            | y::accs ->
                match x,y with
                | Value x, Value y when x + 1L = y ->
                    (Range { From = x; To = y })::accs
                | Value x, Range y when x + 1L = y.From ->
                    (Range { From = x; To = y.To })::accs
                | Value x, Range y when Range.containsValue x y ->
                    (Range { From = x; To = y.To })::accs
                | Range x, Value y when x.To = y || x.To + 1L = y ->
                    (Range { From = x.From; To = y })::accs
                | Range x, Value y when Range.containsValue y x ->
                    (Range { From = x.From; To = y })::accs
                | Range x, Range y when x.To + 1L = y.From ->
                    (Range { From = x.From; To = y.To })::accs
                | Range x, Range y when Option.isSome (Range.tryIntersect x y) ->
                    (Range (Range.union x y))::accs
                | x, Range { From = yFrom; To = yTo } when yFrom = yTo -> x::Value yTo::accs
                | x, y -> x::y::accs)

    let rec simplifyNoRange cnstrnt =
        match cnstrnt with
        | Value _ -> cnstrnt
        | OneOf [x] -> x
        | OneOf lst ->
            match (simplifyListNoRange lst) with
            | [x] -> x
            //| lst -> OneOf (List.sort lst)
            | lst -> OneOf (List.sort lst)

    and simplifyListNoRange lst : Constraint List =
        // flatten one-ofs that are nested
        let lst =
            lst |> List.collect (function
                | OneOf innerLst -> innerLst
                | x -> [x])

        // simplify where there are multiple values that are the same
        lst |> List.distinct


let rec propagateAddInstr a b =
    match a, b with
    | Value aValue, Value bValue -> Value (aValue + bValue)
    | Range { From = aFrom; To = aTo }, Value bValue ->
        Range { From = aFrom + bValue; To = aTo + bValue }
    | Value aValue, Range { From = bFrom; To = bTo }  ->
        Range { From = aValue + bFrom; To = aValue + bTo }
    | Range aRange, Range bRange  ->
        Range (Range.add aRange bRange)
    | OneOf lstA, OneOf lstB ->
        List.allPairs lstA lstB
        |> List.map (fun (a, b) -> propagateAddInstr a b)
        |> OneOf
        |> Constraints.simplify
    | a, OneOf lst ->
        lst
        |> List.map (fun b -> propagateAddInstr a b)
        |> OneOf
        |> Constraints.simplify
    | OneOf lst, b ->
        lst
        |> List.map (fun a -> propagateAddInstr a b)
        |> OneOf
        |> Constraints.simplify

let rec propagateMulInstr a b =
    match a, b with
    | Value 0L, _
    | _, Value 0L -> Value 0L
    | a, Value 1L -> a
    | Value 1L, b -> b
    | Value aValue, Value bValue -> Value (aValue * bValue)
    | Range { From = aFrom; To = aTo }, Value bValue ->
        // When we multiply a range by a number it becomes discontinuous, so
        // we need to break it up into a set of OneOfs
        // Note: For this problem set and knowing we will do back propagation,
        // we might just be able to keep it to the larger range.. not sure
        [ for a in aFrom..aTo ->
             Value (a * bValue) ] |> OneOf
    | Value aValue, Range { From = bFrom; To = bTo } ->
        [ for b in bFrom..bTo ->
             Value (aValue * b) ] |> OneOf
    | Range { From = aFrom; To = aTo }, Range { From = bFrom; To = bTo } ->
        [ for a in aFrom..aTo ->
             [ for b in bFrom..bTo ->
                 Value (a * b) ] ] |> List.collect id |> OneOf
        |> Constraints.simplify
    | Value a, OneOf lst ->
        lst
        |> List.map (fun b -> propagateMulInstr (Value a) b)
        |> OneOf
        |> Constraints.simplify
    | OneOf lst, Value b ->
        lst
        |> List.map (fun a -> propagateMulInstr a (Value b))
        |> OneOf
        |> Constraints.simplify
    | Range { From = aFrom; To = aTo }, OneOf lst ->
        let aLst = [ for a in aFrom..aTo -> Value a ] |> OneOf
        propagateMulInstr aLst (OneOf lst)
        |> Constraints.simplify
    | OneOf lst, Range { From = bFrom; To = bTo } ->
        let bLst = [ for b in bFrom..bTo -> Value b ] |> OneOf
        propagateMulInstr (OneOf lst) bLst
        |> Constraints.simplify
    | OneOf lstA, OneOf lstB ->
        List.allPairs lstA lstB
        |> List.map (fun (a, b) -> propagateMulInstr a b)
        |> OneOf
        |> Constraints.simplify

let rec propagateDivInstr a b =
    match a, b with
    | _, Value 0L -> failwith "Can't divide by 0"
    | a, Value 1L -> a
    | Value aValue, Value bValue -> Value (aValue / bValue)
    | Range { From = aFrom; To = aTo }, Value bValue ->
        // When we divide a range by a number it remains continuous, the to/from
        // order can swap though
        { From = aFrom / bValue; To = aTo / bValue }
        |> Range.ensureOrder
        |> Range
    | Value aValue, Range range  ->
        Range (Range.valueDiv aValue range)
    | Range aRange, Range bRange ->
        Range (Range.div aRange bRange)
    | OneOf lstA, OneOf lstB ->
        List.allPairs lstA lstB
        |> List.map (fun (a, b) -> propagateDivInstr a b)
        |> OneOf
        |> Constraints.simplify
    | a, OneOf lst ->
        lst
        |> List.map (fun b -> propagateDivInstr a b)
        |> OneOf
        |> Constraints.simplify
    | OneOf lst, b ->
        lst
        |> List.map (fun a -> propagateDivInstr a b)
        |> OneOf
        |> Constraints.simplify

let rec propagateModInstr a b =
    match a, b with
    | _, Value x when x < 1 -> failwith "Can't modulo when b is less than 1"
    | Value x, _  when x < 0 -> failwith "Can't modulo when a less than 0"
    | Value 0L, _ -> Value 0L // 0 mod anything is 0
    | Value x, Value y when x < y -> Value x
    | Value x, Value y -> Value (x % y)
    | Range range, Value bValue ->
        OneOf (Range.modValue range bValue |> List.map Range)
        |> Constraints.simplify
    | Value aValue, Range range  ->
        OneOf (Range.valueMod aValue range |> List.map Range)
        |> Constraints.simplify
    | Range aRange, Range bRange ->
        OneOf (Range.modRanges aRange bRange |> List.map Range)
        |> Constraints.simplify
    | OneOf lstA, OneOf lstB ->
        List.allPairs lstA lstB
        |> List.map (fun (a, b) -> propagateModInstr a b)
        |> OneOf
        |> Constraints.simplify
    | a, OneOf lst ->
        lst
        |> List.map (fun b -> propagateModInstr a b)
        |> OneOf
        |> Constraints.simplify
    | OneOf lst, b ->
        lst
        |> List.map (fun a -> propagateModInstr a b)
        |> OneOf
        |> Constraints.simplify

let rec propagateEqlInstr a b =
    let allValuesRange = Range { From = 0; To = 1 }
    match a, b with
    | Value x, Value y when x = y -> Value 1L
    | Value _, Value _ -> Value 0L
    | Range aRange, Value b ->
        if { From = b; To = b} = aRange then Value 1L
        elif Range.containsValue b aRange then allValuesRange
        else Value 0L
    | Value a, Range bRange ->
        if { From = a; To = a} = bRange then Value 1L
        elif Range.containsValue a bRange then allValuesRange
        else Value 0L
    | Range aRange, Range bRange ->
        if aRange = bRange && Range.size aRange = 1L then Value 1L
        elif Range.tryIntersect aRange bRange = None then Value 0L
        else allValuesRange
    | a, OneOf lst ->
        lst
        |> List.map (fun b -> propagateEqlInstr a b)
        |> List.distinct
        |> fun lst ->
            if lst = [ Value 0L ] then Value 0L
            elif lst = [ Value 1L ] then Value 1L
            elif Set lst = Set [ Value 0L; Value 1L ] then allValuesRange
            elif List.exists ((=) allValuesRange) lst then allValuesRange
            else failwithf "Impossible result a: %A, b: %A" a b
    | OneOf lst, b ->
        lst
        |> List.map (fun a -> propagateEqlInstr a b)
        |> List.distinct
        |> fun lst ->
            if lst = [ Value 0L ] then Value 0L
            elif lst = [ Value 1L ] then Value 1L
            elif Set lst = Set [ Value 0L; Value 1L ] then allValuesRange
            elif List.exists ((=) allValuesRange) lst then allValuesRange
            else failwithf "Impossible result a: %A, b: %A" a b

// I'm not sure propogating down going to work? We can't use things like
// div and mod to restrict previous ranges...

let setVarConstraint varName cnstrnt constraints =
    match varName with
    | VarW -> { constraints with W = cnstrnt }
    | VarX -> { constraints with X = cnstrnt }
    | VarY -> { constraints with Y = cnstrnt }
    | VarZ -> { constraints with Z = cnstrnt }

//let possibleInputs = OneOf [ for i in 1..9 -> Value i ]
let possibleInputs = Range { From = 1; To = 9 }

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
    | OneOf lstA, Value b -> (lstA |> List.find (fun a -> a = Value b)), Value b
    | Value a, OneOf lstB -> Value a, (lstB |> List.find (fun b -> b = Value a))
    | OneOf lstA, OneOf lstB ->
        let intersection = Set.intersect (Set lstA) (Set lstB) |> List.ofSeq
        OneOf intersection, OneOf intersection

let reduceToNotEquals a b =
    match a, b with
    | Value a, Value b when a <> b -> Value a, Value b
    | Value a, Value b -> failwith "No valid NOT equals possibility when both are different values"
    | OneOf lstA, Value b -> OneOf (lstA |> List.filter (fun a -> a <> Value b)), Value b
    | Value a, OneOf lstB -> Value a, OneOf (lstB |> List.filter (fun b -> b <> Value a))
    | OneOf lstA, OneOf lstB ->
        // Unless we explore other universes there's not much we can do
        // if there's multiple of both, then we could always pick a combination
        // that's not equal
        OneOf lstA, OneOf lstB

let propagateBackEqlInstr exp a b =
    // We need to set a and b, such that we get exp from a = b
    match exp with
    | Value 0L -> reduceToNotEquals a b
    | Value 1L -> reduceToEquals a b // we need to find matching as in b
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
        | Range a, Value b ->
            let aExp = exp - b
            if Range.containsValue aExp a
            then Value aExp, Value b
            else failwith "No value in range a where a + b can equal c"
        | Value a, Range b ->
            let bExp = exp - a
            if Range.containsValue bExp b
            then Value a, Value bExp
            else failwith "No value in range b where a + b can equal c"
        | Range a, Range b ->
            Range.toValues a
            |> List.choose (fun a ->
                let bExp = exp - a
                if Range.containsValue bExp b
                then Some (Value a, Value bExp)
                else None)
            |> List.unzip
            |> fun (a, b) -> OneOf a, OneOf b
        | OneOf lst, Value b ->
            (lst |> List.choose (fun a ->
                match a with
                | Value a ->
                    if a + b = exp
                    then Some (Value a)
                    else None
                | Range a ->
                    let aExp = exp - b
                    if Range.containsValue aExp a
                    then Some (Value aExp)
                    else None) |> OneOf), Value b
        | Value a, OneOf lst -> Value a, (lst |> List.find (fun (Value x) -> a + x = exp))
        | OneOf lstA, OneOf lstB ->
            // find all combinations where a + b could equal exp
            List.allPairs lstA lstB
            |> List.choose (fun (Value a, Value b) ->
                if a + b = exp
                then Some (Value a, Value b)
                else None)
            //|> List.map (tee (printfn "add: %A"))
            |> List.unzip
            |> fun (lstA, lstB) ->
                OneOf (List.distinct lstA), OneOf (List.distinct lstB)

    | OneOf oneOfLst ->
        match a, b with
        | Value _, Value _ -> failwithf "Adding two values can't equal a range of values"
        | OneOf lst, Value 0L when OneOf lst = exp ->
            OneOf lst, Value 0L
        | OneOf lst, Value b ->
            // the result should be equal to oneOfLst
            let oneOfLst = Set oneOfLst // use a set for performance
            lst |> List.filter (fun (Value x) -> oneOfLst |> Set.contains (Value (x + b))) |> OneOf,
            (Value b)
        | Value 0L, OneOf lst when OneOf lst = exp ->
            Value 0L, OneOf lst
        | Value a, OneOf lst ->
            printfn "%A, %A" (List.length oneOfLst) (List.length lst)
            let oneOfLst = Set oneOfLst // use a set for performance
            (Value a),
            lst |> List.filter (fun (Value x) -> oneOfLst |> Set.contains (Value (a + x))) |> OneOf
        | OneOf lstA, OneOf lstB ->
            // we could split into different universes of possibilities for each
            // valid combination. For now lets just keep them all together and
            // see if it works out
            let oneOfLst = Set oneOfLst // use a set for performance
            let allViable =
                List.allPairs lstA lstB
                |> List.filter (fun (Value a, Value b) ->
                    oneOfLst |> Set.contains (Value (a + b)))
            let a, b = allViable |> List.unzip
            (OneOf a) |> Constraints.simplifyNoRange,
            (OneOf b) |> Constraints.simplifyNoRange

let propagateBackMulInstr exp a b =
    // We need to set a and b, such that we get exp from a * b
    match exp with
    | Value exp ->
        match a, b with
        | Value a, Value b when a * b = exp -> Value a, Value b
        | Value a, Value b -> failwith "a * b cannot equal c"
        | OneOf lstA, Value b -> (lstA |> List.filter (fun (Value a) -> exp = a * b ) |> OneOf), Value b
        | Value a, OneOf lstB -> Value a, (lstB |> List.filter (fun (Value b) -> exp = a * b ) |> OneOf)
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
        | OneOf lstA, Value b ->
            let oneOfLst = Set oneOfLst // use a set for performance
            // the result should be equal to oneOfLst
            lstA |> List.filter (fun (Value a) -> oneOfLst |> Set.contains (Value (a * b))) |> OneOf,
            (Value b)
        | Value a, OneOf lstB ->
            let oneOfLst = Set oneOfLst // use a set for performance
            (Value a),
            lstB |> List.filter (fun (Value b) -> oneOfLst |> Set.contains (Value (a * b))) |> OneOf
        | OneOf lstA, OneOf lstB ->
            // we could split into different universes of possibilities for each
            // valid combination. For now lets just keep them all together and
            // see if it works out
            let oneOfLst = Set oneOfLst // use a set for performance
            let allViable =
                List.allPairs lstA lstB
                |> List.filter (fun (Value a, Value b) ->
                    oneOfLst |> Set.contains (Value (a * b)))
            let a, b = allViable |> List.unzip
            (OneOf a) |> Constraints.simplifyNoRange,
            (OneOf b) |> Constraints.simplifyNoRange

let propagateBackDivInstr exp a b =
    // We need to set a and b, such that we get exp from a / b
    match exp with
    | Value exp ->
        match a, b with
        | Value a, Value b when b <> 0 && exp = a / b -> Value a, Value b
        | Value a, Value b -> failwith "a / b cannot equal c"
        | OneOf lstA, Value b -> (lstA |> List.filter (fun (Value a) -> b <> 0 && a / b = exp) |> OneOf), Value b
        | Value a, OneOf lstB -> Value a, (lstB |> List.filter (fun (Value b) -> b <> 0 && exp = a / b) |> OneOf)
        | OneOf lstA, OneOf lstB ->
            // find all combinations where a / b could equal exp
            List.allPairs lstA lstB
            |> List.choose (fun (Value a, Value b) ->
                if b <> 0 && exp = a / b
                then Some (Value a, Value b)
                else None)
            |> List.unzip
            |> fun (lstA, lstB) ->
                OneOf (List.distinct lstA), OneOf (List.distinct lstB)

    | OneOf oneOfLst ->
        match a, b with
        | Value _, Value _ -> failwithf "Dividing two values can't equal a range of values"
        | OneOf lstA, Value b ->
            let oneOfLst = Set oneOfLst // use a set for performance
            // the result should be equal to oneOfLst
            lstA |> List.filter (fun (Value a) -> oneOfLst |> Set.contains (Value (a / b))) |> OneOf,
            (Value b)
        | Value a, OneOf lstB ->
            let oneOfLst = Set oneOfLst // use a set for performance
            (Value a),
            lstB |> List.filter (fun (Value b) -> oneOfLst |> Set.contains (Value (a / b))) |> OneOf
        | OneOf lstA, OneOf lstB ->
            // we could split into different universes of possibilities for each
            // valid combination. For now lets just keep them all together and
            // see if it works out
            let oneOfLst = Set oneOfLst // use a set for performance
            let allViable =
                List.allPairs lstA lstB
                |> List.filter (fun (Value a, Value b) ->
                    oneOfLst |> Set.contains (Value (a / b)))
            let a, b = allViable |> List.unzip
            (OneOf a) |> Constraints.simplifyNoRange,
            (OneOf b) |> Constraints.simplifyNoRange

let propagateBackModInstr exp a b =
    // We need to set a and b, such that we get exp from a % b
    match exp with
    | Value exp ->
        match a, b with
        | Value a, Value b when exp = a % b -> Value a, Value b
        | Value a, Value b -> failwith "a % b cannot equal c"
        | OneOf lstA, Value b -> (lstA |> List.filter (fun (Value a) -> exp = a % b ) |> OneOf), Value b
        | Value a, OneOf lstB -> Value a, (lstB |> List.filter (fun (Value b) -> exp = a % b) |> OneOf)
        | OneOf lstA, OneOf lstB ->
            // find all combinations where a % b could equal exp
            List.allPairs lstA lstB
            |> List.choose (fun (Value a, Value b) ->
                if exp = a % b
                then Some (Value a, Value b)
                else None)
            |> List.unzip
            |> fun (lstA, lstB) ->
                OneOf (List.distinct lstA), OneOf (List.distinct lstB)

    | OneOf oneOfLst ->
        match a, b with
        | Value _, Value _ -> failwithf "Modding two values can't equal a range of values"
        | OneOf lstA, Value b ->
            // the result should be equal to oneOfLst
            lstA |> List.filter (fun (Value a) -> oneOfLst |> List.contains (Value (a % b))) |> OneOf,
            (Value b)
        | Value a, OneOf lstB ->
            (Value a),
            lstB |> List.filter (fun (Value b) -> oneOfLst |> List.contains (Value (a % b))) |> OneOf
        | OneOf lstA, OneOf lstB ->
            // we could split into different universes of possibilities for each
            // valid combination. For now lets just keep them all together and
            // see if it works out
            let allViable =
                List.allPairs lstA lstB
                |> List.filter (fun (Value a, Value b) ->
                    oneOfLst |> List.contains (Value (a % b)))
            let a, b = allViable |> List.unzip
            (OneOf a) |> Constraints.simplifyNoRange,
            (OneOf b) |> Constraints.simplifyNoRange



let printConstraint startingLineNo lineNo cnstrnt =
    printfn "LineNo: %A" (lineNo + startingLineNo)
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


let performBackInstr instructionFn fwdInstrFn constraints expectCnstrnt varName varOrConst =
    let expect = (Constraints.var varName expectCnstrnt)
    let a, (b : Constraint) =
        instructionFn (Constraints.var varName expectCnstrnt)
                      (Constraints.var varName constraints)
                      (Constraints.varOrConst varOrConst constraints)
    let a = Constraints.simplifyNoRange a

    printfn "Calculated Next %A to be %A" varName a
    match varOrConst with
    | Var name -> printfn "Constrained Next %A to be %A" name b
    | _ -> ()

    // If there are multiple possible expected values, when we find the values
    // for a and b that would make expect, they certinaly shouldn't make a
    // subset of what we know (expect) the result to be. They need to at least
    // be a superset for the backwards propagation to work. And a superset is
    // needed in some cases.
    // e.g.
    // for EQL, we could have a step: eq X W
    // We could expect: X = 1
    // where W is [1..9] (as always)
    // X needs to be any value from 1-9 for X to be able to equal W in any
    // circumstance. We can't know which so when we run the constraint forwards
    // we get X = [0, 1]. It's ok that that's a super set of expected values

    let fwdResult = fwdInstrFn a b |> expandRangeConstraints
    if not (Constraints.isVarSuperSet fwdResult expect) then
        failwithf "Expect is not a subset of fwdResult, expect: %A fwdResult: %A" expect fwdResult

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
        let b = Constraints.simplifyNoRange b

        setVarConstraint varName b constraints
    | Const _ -> constraints
    |> tee printBackwardsStepNext

let rec propagateBackwardsStep history expectCnstrnt acc =
    match history with
    | _ when List.length history < 160 -> acc //failing on 170 // obviously wrong from 177 onwards
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
                    propagateAddInstr
                    constraints expectCnstrnt varName varOrConst
            propagateBackwardsStep history nextCstrnt acc
        | Mul (varName, varOrConst) ->
            let nextCstrnt =
                performBackInstr
                    propagateBackMulInstr
                    propagateMulInstr
                    constraints expectCnstrnt varName varOrConst
            propagateBackwardsStep history nextCstrnt acc
        | Div (varName, varOrConst) ->
            let nextCstrnt =
                performBackInstr
                    propagateBackDivInstr
                    propagateDivInstr
                    constraints expectCnstrnt varName varOrConst
            propagateBackwardsStep history nextCstrnt acc
        | Mod (varName, varOrConst) ->
            let nextCstrnt =
                performBackInstr
                    propagateBackModInstr
                    propagateModInstr
                    constraints expectCnstrnt varName varOrConst
            propagateBackwardsStep history nextCstrnt acc
        | Eql (varName, varOrConst) ->
            let nextCstrnt =
                performBackInstr
                    propagateBackEqlInstr
                    propagateEqlInstr
                    constraints expectCnstrnt varName varOrConst
            propagateBackwardsStep history nextCstrnt acc

// when propagating backwards we are constraining some values based on
// the calculation if we have a + b = 0, and we work out a has to be 0 and
// b has to be 0, then we need to carry that forwards.
//
// My confusion is we're changing the expected value, and the input value at the
// same time if we change the previous constraint. And that isn't right, but
// if we don't carry the change to b forwards, we're just ignoring the constraint
// that we know about

let expandInstructionConstraints cnstrts =
    { W = expandRangeConstraints cnstrts.W
      X = expandRangeConstraints cnstrts.X
      Y = expandRangeConstraints cnstrts.Y
      Z = expandRangeConstraints cnstrts.Z }

let propagateBackwards instructions constraints =
    // The model number is valid if Z has a 0, so we start by enforcing that
    // constraint

    // We should do a sanity check to see if last Z contains a zero. If it doesn't
    // then something's gone quite wrong

    //Convert all Ranges to values
    let constraints = constraints |> List.map expandInstructionConstraints

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
    // instruction 198 is super slow
    // add z y
    // Y has a lot of duplicates, and could be a simple range?? - lets look into that
    let instructions = instructions // |> List.take 40

    let forwardConstraints =
        (Constraints.init, instructions)
        ||> List.scan propagateForwards

    //let printFrom = 249
    //forwardConstraints
    //|> List.skip printFrom
    ////|> List.take 5
    //|> List.iteri (printConstraint printFrom)

    //printfn "Last Z:"
    //forwardConstraints |> List.last |> (fun x -> match x.Z with | OneOf zs -> zs|> List.last | _ -> Value 0L) |> printfn "%A"

    let result =
        propagateBackwards
               (instructions |> List.rev)
               (forwardConstraints |> List.rev)

    Seq.toString "\n" result

    //[]

    // Correct Answer: 580012 took: 37ms

let input = readLinesWithSlashComments "day24.txt" |> parseInput

Helper.measurePart1 part1 input


// Failing at line 170, but W expectation before then is [], so that's probably
// the real problem
