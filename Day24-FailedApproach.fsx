#load "./Helper.fsx"
#load "./Range.fsx"
open Helper
open Range

// Day 24: Arithmetic Logic Unit
//
// A failed approach:
//
// The approach here was to go through each instruction in the input, and use
// the values + knowledge of what the w values could be to constrain the possible
// values. That's the forward propogation part.
//
// Then after doing that forward propogation part we could take the knowledge
// that we had to have a Z = 0 at the end, and propogate that constraint
// backwards.
//
// That will have given us the final constraints for what each input could be,
// and we could simply pick the largest.
//
// In theory that should work, but it had 2 major problems:
// 1 - I Ran into performance issues with the huge possibility state that z
//     could have
// 2 - I think there is a difficult to resolve bug somewhere causing the constraints
//     to come out wrong
//
// I think I could work around these eventually. For performance if I switched Z
// to a base 26 number I could have found a way to improve it. Also I think
// rather than stick to a single state space containing every possibility, if I
// had multiple states (based on the eql comparisons perhaps) I could
// simplify things as well, and reduce the amount of items needed to check.

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
    // TODO: This should be a Set perhaps!
    // Or a set of ranges + a set of Values
    | OneOf of Constraint List

let rec splitOneOfListToRangesAndValues oneOfList =
   (oneOfList, ([], []))
   ||> List.foldBack (fun item (valuesAcc, rangesAcc) ->
       match item with
       | Value x -> x::valuesAcc, rangesAcc
       | Range xr -> valuesAcc, xr::rangesAcc
       | OneOf xs ->
           let values, ranges = splitOneOfListToRangesAndValues xs
           values@valuesAcc, ranges@rangesAcc)

let rec oneOfListToValues oneOfList =
   oneOfList
   |> List.collect (function
       | Value x -> [x]
       | Range x -> Range.toValues x
       | OneOf lst -> oneOfListToValues lst)

let rec flattenOneOfList oneOfList =
   oneOfList
   |> List.collect (function
       | Value x -> [Value x]
       | Range x -> [Range x]
       | OneOf lst -> flattenOneOfList lst)

let rec separateOneOfList oneOfList =
    flattenOneOfList oneOfList
    |> List.map (function
        | Value x -> Some x, None
        | Range x -> None, Some x
        | OneOf _ -> failwithf "Should not have oneof list")
    |> List.unzip
    |> fun (x, r) -> x |> List.choose id, r |> List.choose id

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

    let isEmpty cnstrnt =
        match cnstrnt with
        | OneOf [] -> true
        | _ -> false

    // does A contain each value in B
    let rec isVarSuperSet cnstrntA cnstrntB =
        //printfn "cnstrntA: %A -- cnstrntB: %A" cnstrntA cnstrntB
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
                // 200_000_000L needed for line 139 to work both ways
                // Line 175 is much worse! (last one before we start dividing again)
                | Value x when x > 200_000_000L -> false
                | Range { From = x } when x > 200_000_000L -> false
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
    | Value _, Value _ -> failwith "No valid NOT equals possibility when both are different values"
    | Range a, Value b ->
        if a |> Range.containsValue b
        then OneOf (Range.removeValue b a |> List.map Range), Value b // split the range on b
        else Range a, Value b
    | Value a, Range b ->
        if b |> Range.containsValue a
        then Value a, OneOf (Range.removeValue a b |> List.map Range)  // split the range on b
        else Value a, Range b
    | Range a, Range b ->
        // the only simplification we can make is when a range is just for a
        // single value. If both a and b are for multiple values, every value
        // has a combination that is possible
        match a, b with
        | a, { From = x1; To = x2 } when x1 = x2 ->
            OneOf (Range.removeValue x1 a |> List.map Range), Value x1
        | { From = x1; To = x2 }, b when x1 = x2 ->
            Value x1, OneOf (Range.removeValue x1 b |> List.map Range)
        | a, b -> Range a, Range b
    | OneOf lstA, Value b -> OneOf (lstA |> List.filter (fun a -> a <> Value b)), Value b
    | Value a, OneOf lstB -> Value a, OneOf (lstB |> List.filter (fun b -> b <> Value a))
    | OneOf lstA, OneOf lstB ->
        // Unless we explore other universes there's not much we can do
        // if there's multiple of both, then we could always pick a combination
        // that's not equal
        OneOf lstA, OneOf lstB

let propagateOneOfBackInstr propagateBackAddInstr expLst a b =
    expLst
    |> List.choose (fun exp -> propagateBackAddInstr exp a b)
    |> List.unzip
    |> fun (a, b) -> Constraints.simplify (OneOf a), Constraints.simplify (OneOf b)
    |> function
        | OneOf [], OneOf [] -> None
        | OneOf [], _ | _, OneOf [] -> failwith "Partially empty constraint"
        | x -> Some x

let propagateOneOfABackInstr propagateBackAddInstr exp aLst b =
    aLst
    |> List.choose (fun a ->
        propagateBackAddInstr exp a b)
    |> List.unzip
    |> fun (a, b) -> Constraints.simplify (OneOf a), Constraints.simplify (OneOf b)
    |> function
        | OneOf [], OneOf [] -> None
        | OneOf [], _ | _, OneOf [] -> failwith "Partially empty constraint"
        | x -> Some x

let propagateOneOfBBackInstr propagateBackAddInstr exp a bLst =
    bLst
    |> List.choose (fun b ->
        propagateBackAddInstr exp a b)
    |> List.unzip
    |> fun (a, b) -> Constraints.simplify (OneOf a), Constraints.simplify (OneOf b)
    |> function
        | OneOf [], OneOf [] -> None
        | OneOf [], _ | _, OneOf [] -> failwith "Partially empty constraint"
        | x -> Some x

let propagateBackEqlInstr exp a b =
    // We need to set a and b, such that we get exp from a = b
    match exp with
    | Value 0L
    | Range { From = 0L; To = 0L } -> reduceToNotEquals a b
    | Value 1L
    | Range { From = 1L; To = 1L }  -> reduceToEquals a b // we need to find matching as in b
    | Range { From = 0L; To = 1L }
    | OneOf [ Value 0L; Value 1L ] ->
        // We can get a 0 and a 1 expectation only if a and b can be combined in
        // different ways. Let's just assume they can be to make things easier
        a, b
    | _ -> failwith "An equals result should be 0 or 1"
    |> Some

let rec propagateBackAddInstr exp a b =
    // We need to set a and b, such that we get exp from a + b
    match exp with
    | Value exp ->
        match a, b with
        | Value a, Value b when a + b = exp -> Some (Value a, Value b)
        | Value a, Value b -> None //"a + b cannot equal c"
        | Range a, Value b ->
            let aExp = exp - b
            if Range.containsValue aExp a
            then Some (Value aExp, Value b)
            else None // "No value in range a where a + b can equal c"
        | Value a, Range b ->
            let bExp = exp - a
            if Range.containsValue bExp b
            then Some (Value a, Value bExp)
            else None //"No value in range b where a + b can equal c"
        | Range a, Range b ->
            Range.toValues a
            |> List.choose (fun a ->
                let bExp = exp - a
                if Range.containsValue bExp b
                then Some (Value a, Value bExp)
                else None)
            |> List.unzip
            |> fun (a, b) -> Some (OneOf a, OneOf b)
        | OneOf lstA, Value b ->
            let resultsA, resultsB =
                lstA
                |> List.choose (fun a -> propagateBackAddInstr (Value exp) a (Value b))
                |> List.unzip
                |> fun (a, b) -> Constraints.simplify (OneOf a), Constraints.simplify (OneOf b)

            match resultsB with
            | Value b -> Some (resultsA, resultsB)
            | _ -> failwithf "B must remain as Value B"
        | Value a, OneOf lstB ->
            let resultsA, resultsB =
                lstB
                |> List.choose (fun b -> propagateBackAddInstr (Value exp) (Value a) b)
                |> List.unzip
                |> fun (a, b) -> Constraints.simplify (OneOf a), Constraints.simplify (OneOf b)

            match resultsA with
            | Value a -> Some (resultsA, resultsB)
            | _ -> failwithf "A must remain as Value A"

        | a, OneOf lstB ->
            lstB
            |> List.choose (fun b -> propagateBackAddInstr (Value exp) a b)
            |> List.unzip
            |> fun (a, b) -> Constraints.simplify (OneOf a), Constraints.simplify (OneOf b)
            |> function
                | OneOf [], OneOf [] -> None
                | OneOf [], _ | _, OneOf [] -> failwith "Partially empty constraint"
                | x -> Some x

        | OneOf lstA, b ->
            lstA
            |> List.choose (fun a -> propagateBackAddInstr (Value exp) a b)
            |> List.unzip
            |> fun (a, b) -> Constraints.simplify (OneOf a), Constraints.simplify (OneOf b)
            |> function
                | OneOf [], OneOf [] -> None
                | OneOf [], _ | _, OneOf [] -> failwith "Partially empty constraint"
                | x -> Some x

    | Range rangeExp ->
        match a, b with
        | Value a, Value b when rangeExp.From = a + b && rangeExp.To = a + b ->
            Some (Value a, Value b)
        | Value a, Value b ->
            if Range.containsValue (a + b) rangeExp
            then Some (Value a, Value b)
            else None
        | Value a, Range bRange ->
            let combinedRange = { From = bRange.From + a; To = bRange.To + a }
            match Range.tryIntersect rangeExp combinedRange with
            | Some r ->
                // partial to a full match. Get the parts that match
                Some (Value a, Range { From = r.From - a; To = r.To - a })
            | None -> None
        | Range aRange, Value b ->
            let combinedRange = { From = aRange.From + b; To = aRange.To + b }
            match Range.tryIntersect rangeExp combinedRange with
            | Some r ->
                // partial to a full match. Get the parts that match
                Some (Range { From = r.From - b; To = r.To - b }, Value b)
            | None -> None
        | Range aRange, Range bRange ->
            let aRangeExtents = Range.subtract rangeExp bRange
            let aRestricted = Range.tryIntersect aRangeExtents aRange

            let bRangeExtents = Range.subtract rangeExp aRange
            let bRestricted = Range.tryIntersect bRangeExtents bRange

            match (aRestricted, bRestricted) with
            | Some rA, Some rB -> Some (Range rA, Range rB)
            | _ -> None

        | a, OneOf lstB ->
            lstB
            |> List.choose (fun b -> propagateBackAddInstr exp a b)
            |> List.unzip
            |> fun (a, b) -> Constraints.simplify (OneOf a), Constraints.simplify (OneOf b)
            |> function
                | OneOf [], OneOf [] -> None
                | OneOf [], _ | _, OneOf [] -> failwith "Partially empty constraint"
                | x -> Some x

        | OneOf lstA, b ->
            lstA
            |> List.choose (fun a -> propagateBackAddInstr exp a b)
            |> List.unzip
            |> fun (a, b) -> Constraints.simplify (OneOf a), Constraints.simplify (OneOf b)
            |> function
                | OneOf [], OneOf [] -> None
                | OneOf [], _ | _, OneOf [] -> failwith "Partially empty constraint"
                | x -> Some x

    | OneOf expLst ->
        match a, b with
        | Value a, Value b when expLst = [ Value (a + b) ] -> Some (Value a, Value b)
        | Value _, Value _ -> failwithf "Adding two values can't equal a list of values"
        | a, Value 0L when a = exp ->
            Some (a, Value 0L)
        | Value 0L, b when b = exp ->
            Some (Value 0L, b)
        | a, b ->
            propagateOneOfBackInstr propagateBackAddInstr expLst a b

let rec propagateBackMulInstr exp a b =
    // We need to set a and b, such that we get exp from a * b
    match exp with
    | Value exp ->
        match a, b with
        | Value a, Value b when a * b = exp -> Some (Value a, Value b)
        | Value a, Value b -> None
        | Range aRange, Value 0L ->
            if exp = 0L then
               Some (Range aRange, Value 0L)
            else None
        | Range aRange, Value b ->
            let next = exp / b
            if Range.containsValue next aRange then
                Some (Value next, Value b)
            else None
        | Range aRange, Range bRange ->
            if exp = 0L then
                if aRange |> Range.containsValue 0L && bRange |> Range.containsValue 0L then
                    // if both ranges contain a 0, then any combination is valid
                    Some (Range aRange, Range bRange)
                elif bRange |> Range.containsValue 0L then
                    // otherwise if only b has a 0, then b has to be 0 and a can be anything
                    Some (Range aRange, Value 0L)
                elif aRange |> Range.containsValue 0L then
                    // otherwise if only a has a 0, then a has to be 0 and b can be anything
                    Some (Value 0L, Range bRange)
                else
                    // if none of them have a 0, then we can't possibly get 0
                    None
            else
                failwithf "Value %d = Range * Range is not implemented" exp
        | Range aRange, OneOf _ ->
            failwithf "Value %d = Range * OneOf is not implemented" exp
        | Value 0L, Range bRange ->
            if exp = 0L then
               Some (Value 0L, Range bRange)
            else None
        | Value a, Range bRange ->
            let next = exp / a
            if Range.containsValue next bRange then
                Some (Value a, Value next)
            else None

        | OneOf _, Range bRange -> failwith "Value = OneOf * Range is not implemented"
        | OneOf lstA, Value b ->
            // TODO: Optimize this one
            let aValues, aRanges = splitOneOfListToRangesAndValues lstA
            let wantA = exp / b

            if exp <> wantA * b
            // because of rounding, division could this could give us a value
            // that isn't quite right. i.e. 15/4 = 3.75, but with floor we get 3
            // so we do an extra check to prevent that
            then None
            elif aValues |> List.contains wantA
            then Some (Value wantA, Value b)
            elif aRanges |> List.exists (Range.containsValue wantA)
            then Some (Value wantA, Value b)
            else None
        | Value a, OneOf lstB ->
            Some (Value a, (lstB |> oneOfListToValues |> List.filter (fun b -> exp = a * b) |> List.map Value |> OneOf |> Constraints.simplify))
        | OneOf lstA, OneOf lstB ->
            lstB
            |> List.choose (fun b -> propagateBackMulInstr (Value exp) (OneOf lstA) b)
            |> List.unzip
            |> fun (a, b) -> Constraints.simplify (OneOf a), Constraints.simplify (OneOf b)
            |> function
                | OneOf [], OneOf [] -> None
                | OneOf [], _ | _, OneOf [] -> failwith "Partially empty constraint"
                | x -> Some x

    | Range rangeExp ->
        match a, b with
        | Value a, Value b ->
            if rangeExp |> Range.containsValue (a * b)
            then Some (Value a, Value b)  // matches a subset of values
            else None
        | Range aRange, Value 0L ->
            if rangeExp |> Range.containsValue 0
            then Some (Range aRange, Value 0L)
            else None
        | Range aRange, Value 1L ->
            match Range.tryIntersect rangeExp aRange with
            | Some intersect -> Some (Range intersect, Value 1L)
            | None -> None
        | Range aRange, Value b ->
            // TODO: Dividing is slow, is there a faster option?
            let divRange = rangeExp |> Range.valueDiv b
            match Range.tryIntersect aRange divRange with
            | Some intersect ->
                Some (Range intersect, Value b)
            | None ->
                None
        | Range aRange, OneOf _ ->
            failwith "RangeExp = Range * OneOf not implemented"
            // multiply them out and see if a subset fits
        | Value 0L, Range bRange ->
            if rangeExp |> Range.containsValue 0
            then Some (Value 0L, Range bRange)
            else None
        | Value 1L, Range bRange ->
            match Range.tryIntersect rangeExp bRange with
            | Some intersect -> Some (Value 1L, Range intersect)
            | None -> None
        | Value a, Range bRange ->
            let divRange = rangeExp |> Range.valueDiv a
            match Range.tryIntersect divRange bRange with
            | Some intersect ->
                Some (Value a, Range intersect)
            | None ->
                None
        | Range aRange, Range bRange ->
            let divRangeB = Range.div rangeExp aRange
            let divRangeA = Range.div rangeExp bRange // should be be dividing by divRangeB?
            match Range.tryIntersect divRangeA aRange, Range.tryIntersect divRangeB bRange with
            | Some intersectA, Some intersectB ->
                Some (Range intersectA, Range intersectB)
            | _ ->
                //failwith "RangeExp = Range * Range has no intersections"
                None
        | OneOf aLst, Value b ->
            let aValues, aRanges = splitOneOfListToRangesAndValues aLst
            let aValues = aValues |> List.filter (fun a -> Range.containsValue (a * b) rangeExp) |> List.map Value
            let aRanges =
                aRanges
                |> List.choose (fun a -> propagateBackMulInstr (Range rangeExp) (Range a) (Value b))
                |> List.unzip |> fst
            Some (aValues @ aRanges |> OneOf |> Constraints.simplify, Value b)
        | OneOf _, Range _ ->
            failwith "RangeExp = OneOf * Range not implemented"
        | OneOf aLst, OneOf bLst ->
            bLst
            |> List.choose (fun b ->
                propagateBackMulInstr (Range rangeExp) (OneOf aLst) b)
            |> List.unzip
            |> fun (a, b) -> Constraints.simplify (OneOf a), Constraints.simplify (OneOf b)
            |> function
                | OneOf [], OneOf [] -> None
                | OneOf [], _ | _, OneOf [] -> failwith "Partially empty constraint"
                | x -> Some x
        | Value _, OneOf _ ->
            failwith "RangeExp = Value * OneOf  not implemented"

    | OneOf expLst ->
        match a, b with
        | Value a, Value b ->
            if List.contains (Value (a * b)) expLst
            then Some (Value a, Value b)
            else None

        | OneOf lstA, Value 1L ->
            if lstA = expLst then
                Some (OneOf lstA, Value 1L)
            else
                // subset of Lst A potentially
                failwithf "OneOf = OneOf * Value not implemented"
        | Value 1L, OneOf lstB ->
            if lstB = expLst then
                Some (Value 1L, OneOf lstB)
            else
                // subset of Lst B potentially
                failwithf "OneOf = Value * OneOf not implemented"
        | Range rangeA, Value b ->
            let expValues, expRanges = splitOneOfListToRangesAndValues expLst

            let valuesResults =
                expValues
                |> List.choose (fun exp ->
                    let wantA = exp / b
                    if wantA * b <> exp then None
                    else
                        if Range.containsValue wantA rangeA
                        then Some (Value wantA, Value b)
                        else None)

            let rangesResults =
                expRanges
                |> List.choose (fun expRange ->
                    propagateBackMulInstr (Range expRange) (Range rangeA) (Value b))

            valuesResults @ rangesResults
            |> List.unzip
            |> fun (a, b) -> Constraints.simplify (OneOf a), Constraints.simplify (OneOf b)
            |> function
                | OneOf [], OneOf [] -> None
                | OneOf [], _ | _, OneOf [] -> failwith "Partially empty constraint"
                | x -> Some x

        | OneOf lstA, Value b ->
            // we'll have to process for each item in a list. Pick the smallest
            // list to iterate on
            if expLst.Length < lstA.Length
            then propagateOneOfBackInstr propagateBackMulInstr expLst a (Value b)
            else propagateOneOfABackInstr propagateBackMulInstr exp lstA (Value b)
        | Value a, OneOf lstB ->
            // we'll have to process for each item in a list. Pick the smallest
            // list to iterate on
            if expLst.Length < lstB.Length
            then propagateOneOfBackInstr propagateBackMulInstr expLst (Value a) b
            else propagateOneOfBBackInstr propagateBackMulInstr exp (Value a) lstB
        | a, b ->
            // might need to break it up for performance in some occasions
            propagateOneOfBackInstr propagateBackMulInstr expLst a b
        //| OneOf lstA, Value b ->
        //    let oneOfLst = Set oneOfLst // use a set for performance
        //    // the result should be equal to oneOfLst
        //    Some (lstA |> List.filter (fun (Value a) -> oneOfLst |> Set.contains (Value (a * b))) |> OneOf,
        //          (Value b))
        //| Value a, OneOf lstB ->
        //    let oneOfLst = Set oneOfLst // use a set for performance
        //    Some ((Value a),
        //          lstB |> List.filter (fun (Value b) -> oneOfLst |> Set.contains (Value (a * b))) |> OneOf)
        //| OneOf lstA, OneOf lstB ->
        //    // we could split into different universes of possibilities for each
        //    // valid combination. For now lets just keep them all together and
        //    // see if it works out
        //    let oneOfLst = Set oneOfLst // use a set for performance
        //    let allViable =
        //        List.allPairs lstA lstB
        //        |> List.filter (fun (Value a, Value b) ->
        //            oneOfLst |> Set.contains (Value (a * b)))
        //    let a, b = allViable |> List.unzip
        //    Some ((OneOf a) |> Constraints.simplify,
        //          (OneOf b) |> Constraints.simplify)

let propagateBackDivInstr exp a b =
    // We need to set a and b, such that we get exp from a / b
    match exp with
    | Value exp ->
        match a, b with
        | Value a, Value b when b <> 0 && exp = a / b -> Value a, Value b
        | Value a, Value b -> failwith "a / b cannot equal c"
        | Range aRange, _ -> failwith "Value = Range / b is not implemented"
        | _, Range bRange -> failwith "Value = a / Range is not implemented"
        | OneOf lstA, Value b ->
            (lstA |> oneOfListToValues |> List.filter (fun a -> b <> 0 && a / b = exp) |> List.map Value |> OneOf), Value b
        | Value a, OneOf lstB ->
            Value a, (lstB |> oneOfListToValues |> List.filter (fun b -> b <> 0 && exp = a / b) |> List.map Value |> OneOf)
        | OneOf lstA, OneOf lstB ->
            // find all combinations where a / b could equal exp

            List.allPairs (lstA |> oneOfListToValues) (lstB |> oneOfListToValues)
            |> List.choose (fun (a, b) ->
                if b <> 0 && exp = a / b
                then Some (Value a, Value b)
                else None)
            |> List.unzip
            |> fun (lstA, lstB) ->
                OneOf (List.distinct lstA) |> Constraints.simplify,
                OneOf (List.distinct lstB) |> Constraints.simplify

    | Range rangeExp ->
        match a, b with
        | Value _, Value _ -> failwithf "Range = Value / Value is not implemented"
        | Range aRange, Value 1L ->
            if rangeExp = aRange then
                Range aRange, Value 1L
            else failwith "Ranges must be equal if dividing by 1"
        | Range aRange, Value _ -> failwith "Range = Range / Value is not implemented"
        | Range aRange, Range _ -> failwith "Range = Range / Range is not implemented"
        | Range aRange, OneOf _ -> failwith "Range = Range / b is not implemented"
        | _, Range bRange -> failwith "Range = a / Range is not implemented"
        | OneOf aLst, _ -> failwith "Range = OneOf / b is not implemented"
        | _, OneOf bLst -> failwith "Range = a / OneOf is not implemented"

    | OneOf oneOfLst ->
        match a, b with
        | Value _, Value _ -> failwithf "Dividing two values can't equal a range of values"
        | Range aRange, _ -> failwith "OneOf = Range / b is not implemented"
        | _, Range bRange -> failwith "OneOf = a / Range is not implemented"
        | OneOf lstA, Value 1L when oneOfLst = lstA -> OneOf lstA, Value 1L
        | OneOf lstA, Value b ->
            let oneOfLst = Set (oneOfLst |> oneOfListToValues) // use a set for performance
            // the result should be equal to oneOfLst
            lstA |> oneOfListToValues |> List.filter (fun a -> oneOfLst |> Set.contains (a / b)) |> List.map Value |> OneOf |> Constraints.simplify,
            (Value b)
        | Value a, OneOf lstB ->
            let oneOfLst = Set (oneOfLst |> oneOfListToValues) // use a set for performance
            (Value a),
            lstB |> oneOfListToValues |> List.filter (fun b -> oneOfLst |> Set.contains (a / b)) |> List.map Value |> OneOf |> Constraints.simplify
        | OneOf lstA, OneOf lstB ->
            // we could split into different universes of possibilities for each
            // valid combination. For now lets just keep them all together and
            // see if it works out
            let oneOfLst = Set (oneOfLst |> oneOfListToValues) // use a set for performance
            let allViable =
                List.allPairs (oneOfListToValues lstA) (oneOfListToValues lstB)
                |> List.filter (fun (a, b) ->
                    oneOfLst |> Set.contains (a / b))
                |> List.map (fun (a, b) -> Value a, Value b)
            let a, b = allViable |> List.unzip
            (OneOf a) |> Constraints.simplify,
            (OneOf b) |> Constraints.simplify
    |> Some

let rec propagateBackModInstr exp a b =
    // We need to set a and b, such that we get exp from a % b
    match exp with
    | Value exp ->
        match a, b with
        | Value a, Value b when exp = a % b -> Some (Value a, Value b)
        | Value a, Value b -> None
        | Range aRange, _ -> failwith "Value = Range mod b is not implemented"
        | _, Range bRange -> failwith "Value = a mod Range is not implemented"
        | OneOf lstA, Value b ->
            let aValues, aRanges = splitOneOfListToRangesAndValues lstA
            let aValues = aValues |> List.filter (fun a -> exp = a % b ) |> List.map Value
            let aRanges =
                aRanges
                |> List.choose (fun aRange -> propagateBackModInstr (Value exp) (Range aRange) (Value b))
                |> List.unzip |> fst
            Some (aValues @ aRanges |> OneOf |> Constraints.simplify, Value b)
        | Value a, OneOf lstB ->
            let bValues, bRanges = splitOneOfListToRangesAndValues lstB
            let bValues = bValues |> List.filter (fun b -> exp = a % b ) |> List.map Value
            let bRanges =
                bRanges
                |> List.choose (fun bRange -> propagateBackModInstr (Value exp) (Value a) (Range bRange) )
                |> List.unzip |> fst
            Some (Value a, bValues @ bRanges |> OneOf |> Constraints.simplify)
        | OneOf lstA, OneOf lstB ->
            lstA
            |> List.choose (fun a -> propagateBackModInstr (Value exp) a (OneOf lstB) )
            |> List.unzip
            |> fun (a, b) -> Constraints.simplify (OneOf a), Constraints.simplify (OneOf b)
            |> function
                | OneOf [], OneOf [] -> None
                | OneOf [], _ | _, OneOf [] -> failwith "Partially empty constraint"
                | x -> Some x

    | Range rangeExp ->
        match a, b with
        | Value _, Value _ -> failwithf "Range = Value mod Value is not implemented"
        | Range aRange, Value b ->
            if rangeExp = aRange then
                Some (Range aRange, Value b)
            elif { From = 0L; To = b - 1L } = rangeExp then
                // if the expected range covers all possible values, then aRange
                // could be anything
                Some (Range aRange, Value b)
            else failwith "Range = Range mod Value is not implemented"
            // we need all the values in aRange that will be in rangeExp when
            // modulus b. The fact that we expect to get a range reduces our
            // search space quite a lot
            // 10 % 6 = 4
        | Range aRange, Range _ -> failwith "Range = Range mod Range is not implemented"
        | Range aRange, OneOf _ -> failwith "Range = Range mod OneOf is not implemented"
        | Value _, Range bRange -> failwith "Range = Value mod Range is not implemented"
        | OneOf _, Range bRange -> failwith "Range = OneOf mod Range is not implemented"
        | OneOf aLst, Value b ->
            if rangeExp = { From = 0L; To = b - 1L } then
                // if the expected result covers everything, than any input
                // is valid
                Some (OneOf aLst, Value b)
            else
                let expSet = rangeExp |> Range.toValues |> Set
                let aLst = oneOfListToValues aLst
                Some (aLst
                        |> List.filter (fun a -> Set.contains (a % b) expSet)
                        |> List.map Value |> OneOf |> Constraints.simplify,
                      Value b)

        | OneOf aLst, OneOf _ -> failwith "Range = OneOf mod OneOf is not implemented"
        | _, OneOf bLst -> failwith "Range = a mod OneOf is not implemented"

    | OneOf expLst ->
        // might need to break it up for performance in some occasions
        propagateOneOfBackInstr propagateBackModInstr expLst a b


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
    let result =
        instructionFn (Constraints.var varName expectCnstrnt)
                      (Constraints.var varName constraints)
                      (Constraints.varOrConst varOrConst constraints)
    match result with
    | None -> failwithf "No possible back step could be found!"
    | Some (a, b : Constraint) ->
        // If there are multiple possible expected values, when we try to find
        // the values for a and b that would make the expected constraint, they
        // certainly shouldn't make a subset of what we know (expect) the result
        // to be. They need to at least be a superset for the backwards
        // propagation to work. A superset is needed in some cases like below.
        // e.g.
        // for EQL, we could have a step: "eq X W"
        // We could expect: X = 1
        // where W is [1..9] (as always)
        // X needs to be any value from 1-9 for X to be able to equal W in any
        // circumstance. We can't know which so when we run the constraint
        // forwards with X as [1..9] and W as [1..9] we get X = [0, 1].
        // It's ok that the result is a super set of expected values

        let fwdResult = fwdInstrFn a b //|> expandRangeConstraints
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
                | Value _, Value _ -> failwith "Constraint Value is incompatible with previous Value"
                | Range b, Value bExp when b |> Range.containsValue bExp -> Value bExp
                | Range _, Value _ -> failwith "Constraint Range is incompatible with previous Value"
                | Value b, Range bExp when bExp |> Range.containsValue b -> Value b
                | Value _, Range _ -> failwith "Constraint Value is incompatible with previous Range"
                | Range b, Range bExp ->
                    match Range.tryIntersect b bExp with
                    | Some intersect -> Range intersect
                    | None -> failwith "Constraint Range is incompatible with previous Range"
                | OneOf bList, Value bExp
                    when bList |> List.contains (Value bExp) -> Value bExp
                | OneOf _, Value _ -> failwith "Constraint List is incompatible with previous Value"
                | Value b, OneOf bExpList
                    when bExpList |> List.contains (Value b) -> Value b
                | Value _, OneOf _ -> failwith "Constraint List is incompatible with previous Value"
                | OneOf bLst, OneOf bExpList ->
                    let intersect = Set.intersect (Set bLst) (Set bExpList)
                    if intersect.Count = 0 then failwith "Constraint List is incompatible with previous List"
                    else intersect |> List.ofSeq |> OneOf
                | Range _, OneOf _ -> failwith "Range b, OneOf bexp isn't currently supported"
                | OneOf _, Range _ -> failwith "OneOf b, Range bExp isn't currently supported"

            let b = Constraints.simplify b

            setVarConstraint varName b constraints
        | Const _ -> constraints

type StepResult =
    | NextStep of Constraints
    | MatchedInput of Constraint

let singleBackwardsStep (instruction, constraints) expectCnstrnt =
    match instruction with
    | Inp varName ->
       let vals = Constraints.var varName expectCnstrnt
       MatchedInput vals
    | Add (varName, varOrConst) ->
        performBackInstr
            propagateBackAddInstr
            propagateAddInstr
            constraints expectCnstrnt varName varOrConst
        |> NextStep
    | Mul (varName, varOrConst) ->
        performBackInstr
            propagateBackMulInstr
            propagateMulInstr
            constraints expectCnstrnt varName varOrConst
        |> NextStep
    | Div (varName, varOrConst) ->
        performBackInstr
            propagateBackDivInstr
            propagateDivInstr
            constraints expectCnstrnt varName varOrConst
        |> NextStep
    | Mod (varName, varOrConst) ->
        performBackInstr
            propagateBackModInstr
            propagateModInstr
            constraints expectCnstrnt varName varOrConst
        |> NextStep
    | Eql (varName, varOrConst) ->
        performBackInstr
            propagateBackEqlInstr
            propagateEqlInstr
            constraints expectCnstrnt varName varOrConst
        |> NextStep

let rec propagateBackwardsStep history expectCnstrnt acc =
    match history with
    | _ when List.length history < 236 -> acc //failing on 170 // obviously wrong from 177 onwards
    | [] -> acc
    | (instruction, constraints)::history ->
        printBackwardsStep
            (List.length history + 1) (instruction, constraints) expectCnstrnt

        match singleBackwardsStep (instruction, constraints) expectCnstrnt with
        | NextStep nextCstrnt ->
            printBackwardsStepNext nextCstrnt
            propagateBackwardsStep history nextCstrnt acc
        | MatchedInput vals ->
            printfn "***** PossibleValues: %A" vals
            propagateBackwardsStep history expectCnstrnt (vals::acc)

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

type TestResult =
    | Matching
    | NoMatch of Instr * Constraints * Constraints * Constraints
    | InputStep


// Tester
let testSingleStep (instructions, forwardConstraints) stepNo =
    let [instruction] = instructions |> List.rev |> List.take 1
    let [final; forwardConstraints] = forwardConstraints |> List.rev |> List.take 2

    //printBackwardsStep stepNo (instruction, forwardConstraints) final

    let backwardsStep = singleBackwardsStep (instruction, forwardConstraints) final

    match backwardsStep with
    | NextStep step ->
        if step = forwardConstraints then
            Matching
        else
            NoMatch (instruction, forwardConstraints, final, step)
    | MatchedInput _ ->
        InputStep

let rec firstNonMatching lstA lstB =
    match lstA, lstB with
    | a::lstA, b::lstB when a <> b -> a,b
    | a::lstA, b::lstB -> firstNonMatching lstA lstB
    | [], [] -> failwithf "all matched"
    | [], _ -> failwithf "Ran out of A to check"
    | _, [] -> failwithf "Ran out of B to check"

let tester instructions =
    let forwardConstraints =
        (Constraints.init, instructions)
        ||> List.scan propagateForwards

    // propagateForwards is slow when we get rid of limits. Where's the
    // performance bottleneck???
    printfn "Completed Forwards Propagation"

    let mutable continueLooping = true
    let mutable stepNo = 155

    // Line 139 NOT MATCHING!!! (due to artificial limit)
    while continueLooping && stepNo < 165 do
        stepNo <- stepNo + 1
        let instructions = instructions |> List.take (stepNo + 0)
        let forwardConstraints = forwardConstraints |> List.take (stepNo + 1)
        let r = testSingleStep (instructions, forwardConstraints) stepNo

        match r with
        | NoMatch (instruction, forwardConstraints, final, step) ->
            printBackwardsStep stepNo (instruction, forwardConstraints) final
            printBackwardsStepNext step
            printfn "NOT MATCHING!!!"
            if step.W <> forwardConstraints.W then
                printfn "W doesn't match"
            elif step.X <> forwardConstraints.X then
                printfn "X doesn't match"
            elif step.Y <> forwardConstraints.Y then
                printfn "Y doesn't match"
            elif step.Z <> forwardConstraints.Z then
                printfn "Z doesn't match"
                printfn "last forwards: %A, last backwards: %A"
                        (forwardConstraints.Z |> function | OneOf lst -> lst |> List.last | _ -> Value 0L)
                        (step.Z |> function | OneOf lst -> lst |> List.last | _ -> Value 0L)
                printfn "forwards length: %d, backwards length: %d"
                        (forwardConstraints.Z |> function | OneOf lst -> lst |> List.length | _ -> 1)
                        (step.Z |> function | OneOf lst -> lst |> List.length | _ -> 1)
                printfn "FirstNonMatching: %A"
                        (firstNonMatching (forwardConstraints.Z |> function | OneOf lst -> lst | _ -> [])
                                          (step.Z |> function | OneOf lst -> lst | _ -> []))
            continueLooping <- false
        | _ ->
            printfn "%d Ok" stepNo


    //let forwardConstraints =
    //    (Constraints.init, instructions)
    //    ||> List.scan propagateForwards

    //let [instruction] = instructions |> List.rev |> List.take 1
    //let [final; forwardConstraints] = forwardConstraints |> List.rev |> List.take 2

    //let backwardsStep = singleBackwardsStep (instruction, forwardConstraints) final

    //printBackwardsStep testStep (instruction, forwardConstraints) final

    //match backwardsStep with
    //| NextStep step ->
    //    if step = forwardConstraints then
    //        printfn "Matching!!!"
    //    else
    //        printfn "NOT MATCHING!!!"
    //        printBackwardsStepNext step
    //| MatchedInput _ ->
    //    printfn "Matched input step"





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

tester input

//Helper.measurePart1 part1 input

// Failing at line 170, but W expectation before then is [], so that's probably
// the real problem
