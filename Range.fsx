#I __SOURCE_DIRECTORY__

type Range = { From: int64; To : int64 }

module Range =

    let containsValue v range =
        v >= range.From && v <= range.To

    let removeValue v range =
        if v <= range.From then [range]
        elif range.To <= v then [range]
        else [ { From = range.From; To = v - 1L }
               { From = v + 1L; To = range.To } ]

    let size range =
        abs(range.To - range.From) + 1L

    let ensureOrder range =
        if range.From > range.To
        then { From = range.To; To = range.From }
        else range

    let tryIntersect { From = aFrom; To = aTo } { From = bFrom; To = bTo } =
        let rFrom = max aFrom bFrom
        let rTo = min aTo bTo
        if rFrom > rTo then None
        else Some { From = rFrom; To = rTo }

    let tryUnion { From = aFrom; To = aTo } { From = bFrom; To = bTo } =
        if (aTo >= bFrom && aFrom <= bTo) || (bTo >= aFrom && bFrom <= aTo) then
            let rFrom = min aFrom bFrom
            let rTo = max aTo bTo
            Some { From = rFrom; To = rTo }
        else
            None

    let union aRange bRange =
        match tryUnion aRange bRange with
        | Some u -> u
        | None -> failwithf "No possible union between: %A and %A" aRange bRange

    let toValues range : int64 List =
        [ for i in range.From..range.To -> i ]

    let fromValues values : Range list =
        let values =
            List.sort values
            |> List.distinct

        (values, [])
        ||> List.foldBack (fun v ranges  ->
            match ranges with
            | [] -> [ { From = v; To = v } ]
            | r::xrs ->
                if r.From - 1L = v
                then { r with From = v }::xrs
                else { From = v; To = v }::r::xrs)

    let simplify (ranges: Range List) : Range List =
        let ranges = ranges |> List.sort
        (ranges, [])
        ||> List.foldBack (fun r ranges  ->
            match ranges with
            | [] -> [ r ]
            | x::xrs ->
                match tryIntersect x r with
                | Some _ -> (union x r)::xrs
                | None -> r::x::xrs)

    /// Finds the potential range we could have when we have to add one number
    /// in a range to a number in another range
    let add { From = aFrom; To = aTo } { From = bFrom; To = bTo } =
        // e.g. [1..3] + [3..6]
        // minimum possible value is 3, largest possible is 9
        // new range is [3..9]
        { From = aFrom + bFrom; To = aTo + bTo }

    /// Finds the potential range we could have when we have to subtract one
    /// number in a range from a number in another range
    /// * Note that this is not the inverse of add! (should it have a different name?)
    let subtract { From = aFrom; To = aTo } { From = bFrom; To = bTo } =
       // e.g. [4..9] - [1..3] = [1..8]
       // minimum possible amount is 1 (4-[1]=3), largest possible is 8 (9-[8]=1)
       { From = aFrom - bTo; To = aTo - bFrom }
       // if we try to add we get a very different result.. both try to find
       // the largest potential range, doing that extends the possibilities
       // [1..8] + [1..3] = [2..11]

    // Gets the resulting range if a value was to be divided by any and all
    // values in the given range.
    // Note: The actual result would be discontinuous for integers, but I'm
    // ignoring that for now. We may need to worry about it later - We could
    // do something simple - If the resulting range would be larger than the
    // input we can check each one
    let valueDiv value { From = bFrom; To = bTo } =
        let denominators =
            if bFrom < 0 && bTo > 0
            then [bFrom; -1; 1; bTo]
            else [bFrom; bTo]

        let potential =
            denominators |> List.map (fun y -> value / y)

        { From = List.min potential; To = List.max potential }

    // Gets the resulting range if any and all values in one range was to be
    // divided by any and all values in another given range.
    // Note: The actual result would be discontinuous for integers, but I'm
    // ignoring that for now. We may need to worry about it later
    let div { From = aFrom; To = aTo } { From = bFrom; To = bTo } =
        // there's probably a better/faster mathematical way of doing this
        let denominators =
            if bFrom < 0 && bTo > 0 then [bFrom; -1; 1; bTo]
            // we try to avoid div by 0 with the next 2
            elif bFrom < 0 && bTo = 0 then [bFrom; -1]
            elif bFrom = 0 && bTo > 0 then [1; bTo]
            else [bFrom; bTo]
            |> List.distinct

        let potential =
            [aFrom; aTo]
            |> List.collect (fun x ->
                denominators
                |> List.map (fun y -> x / y))

        { From = List.min potential; To = List.max potential }

    let modValue { From = aFrom; To = aTo } (value : int64) : Range list =
        if aTo < 0 then []
        elif value <= 0 then []
        else
            let aFrom = max 0L aFrom
            let aTo = max 0L aTo

            // if there are more values in the range, than the number we are
            // mod'ing by, then we would include all possible mod values
            if aTo - aFrom > value
            then [ { From = 0L; To = value - 1L } ]
            else
                // otherwise we would include a subset, that's not too difficult
                // to work out.
                let bFromMod = aFrom % value
                let bToMod = aTo % value

                if bFromMod <= bToMod then
                    [ { From = bFromMod; To = bToMod } ]
                else
                    // from 29 to 36. mod 15
                    // 14, 6
                    [ { From = bFromMod; To = value - 1L }
                      { From = 0; To = bToMod } ]

    let valueMod value { From = bFrom; To = bTo } : Range list =
        if value < 0L then []
        elif bTo <= 0L then []
        else
            let bFrom = max 1L bFrom
            let bTo = max 1L bTo

            let lowerInclude =
                if value < bFrom
                then value + 1L // because: value % (value + 1) = value
                else bFrom

            let upperInclude =
                if value < bTo
                then value + 1L // because: value % (value + 1) = value
                else bTo

            [ for x in lowerInclude..upperInclude -> value % x]
            |> fromValues

    let modRanges aRange { From = aFrom; To = aTo } : Range list =
        [ for x in aFrom..aTo ->
           modValue aRange x ]
        |> List.collect id
        |> simplify

