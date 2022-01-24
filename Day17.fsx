#load "./Helper.fsx"
open Helper

// Day 17 - Trick Shot
//
// Your aim is to lob a sensor into a target area accounting for basic drag and
// gravity.
//
// Remarks: Solved this one algebraically. I imagine it's quite possible with
//          a hill-climbing algorithm or similar, but I found it more "fun"  to
//          break out my high school maths and solve it with a bit of algebra.

let calculateMaxY ((_,_),(_,y2)) =
    // Y can start off positive and get negative so a velocity of 3 will change
    // like so:                             // 3, 2, 1, 0,-1,-2,-3,-4,-5,-6
    // and it's position will change like:  // 3, 5, 6, 6, 5, 3, 0,-4,-9,-15
    //
    // Because it's symmetrical going up and back down it will always
    // hit y = 0 if fired with a y velocity > 0
    //
    // So that means we want to fire it at a velocity which will maximise the
    // difference between 0 and the target area, so it it's the farthest side of
    // the target area.
    //
    // if we take abs(y2) as the velocity we want it to be going when we pass
    // the y=0 axis, can we work back to the initial velocity at our max height?
    // YES.
    // We know the velocity at our highest point must be 0, and the velocity
    // when passing y=0 must be y2. So adding up all those steps from 0 to y2
    // increasing by 1 each time is the max height we must have been.
    // We're just solving for the sum of series from 0 to y2:
    //
    // The number of steps from 0 to a velocity of y2 is y2. That is because we
    // change by 1 step each time so to get from 0 to y2, we have to have y2 steps.
    //
    // Sum of series formula from a with n steps with a step size  of 1 is:
    // n/2(2a + n-1), and a = 0 for us, which gives us:
    ((abs y2)/2)*((abs y2) - 1)

let calculateMaxYSteps y2 =
    // Following the same logic as calculateMaxY we could use sum of series again,
    // and figure out the velocity needed to hit that max y. It's going to be
    // y2 - 1 steps from the vertex (highest point) to the max.
    // But we know that the curve is symetrical, so it will take the same number
    // of steps to reach the vertex, as it does to fall back to 0. So we have the
    // # steps to the vertex.
    let stepsToVertex = (abs y2) - 1

    // The velocity at step n follows the formula a + d * (n - 1) where a is the
    // starting velocity, and d is the step size. a is 0, because velocity is 0
    // at the vertex, and d = 1, because we constantly change by 1 for each step.
    // So our staring velocity is just steps - 1
    let maxInitialYVelocity = stepsToVertex

    // It takes us the the same amount of steps to get back to the 0 y-axis from the
    // vertex. Then there's one step where we're at the vertex with a velocity of 0
    // and not going anywhere. Then we need one more step to actually hit the
    // target. So max steps is 2 * stepsToVertex + 2
    let maxYSteps = 2 * stepsToVertex + 2
    maxYSteps

let calculateXVelocityRange ((x1,_),(x2,_)) =
    // Since it changes by 1 each time, the final x position will be a sum of
    // the series from ending velocity to (ending velocity - number of steps)
    //
    // For the max value we can get to x2 by having an x2 velocity in one step,
    // so that's the max of our x range
    let xVelocityMax = abs x2
    //
    // For the min, we want the x velocity to reduce to 0 when we reach x1. We
    // can find that as a sum of a series from 0 to n where the total will be x1.
    // Sum of series formula from a to n with a step of 1 is: n/2(2a + n-1)
    // Putting 0 for a and rearranging gives us: 0.5*n^2 - 0.5n - 20
    // We can then solve for n using the quadratic formula:
    // (-b +- sqrt(b^2 - 4ac))/2a (we just take the negative case)
    let rangeMinSteps =
        (-(-0.5) - (sqrt ((pown -0.5 2) - (4.*0.5*(float -x1)))))/(1.)
        |> abs |> ceil |> int
    // that gives us the number of steps
    // the actual min value will be the velocity 0 + step size (1: We're
    // calculating backwards) * number of steps. Which is exactly the same
    let xVelocityMin = rangeMinSteps

    (xVelocityMin, xVelocityMax)


let calculateTrajectories ((x1,y1),(x2,y2)) (xVelocityMin, xVelocityMax) =
    // We know the x velocity range, for each possible x velocity determining
    // the y velocities comes down to how many steps we'll potentially have to
    // get in the target zone with each x velocity.
    // If we only have one step, we just have to aim to hit the target on the
    // first step, so a large y. If we have lots of steps we'll want to arc the
    // shot high, so we also have lots of y steps.
    //
    // Can we determine potential x-steps easily?
    // Yes. we can see how many times we could hit a value in the range. That is
    // our step count
    // - so for x velocity of 8 and x1 = 20, x2 = 30;  the sequence will be 8, 15, 21, 26, 30, 33, etc
    // - so we have 3 cases that can hit (21, 26, 30), and it would be between 3 and 5 steps
    //
    // Note we can have an infinite range. For instance  x = 6 is 6, 11, 15, 18, 20, 21, 21, 21, etc
    // So even after 1000 steps we'd still be in the xrange. But as we
    // determined in part 1 there is a max value of y that would work, so we do
    // have an upper limit on steps there.
    //
    // That gives us an upper limit and lower limit on the amount of steps. And
    // we can iterate through it to determine the y velocity.

    let getHitsInRangeForXVelocity xVel (xRangeMin, xRangeMax) =
        let mutable xVel = xVel
        let mutable xn = 0
        let mutable steps = 0
        seq { while xn < xRangeMax do
              xn <- xn + xVel
              if xVel > 0 then xVel <- xVel - 1
              steps <- steps + 1
              if xn >= xRangeMin && xn <= xRangeMax
              then yield (xn, steps) }

    let maxYSteps = calculateMaxYSteps y2

    [ for xVel in xVelocityMin..xVelocityMax do
        getHitsInRangeForXVelocity xVel (x1, x2)
        // the steps and hits for xVelocities are potentially infinite, but
        // since we know the max amount of y steps, we can limit the range
        |> Seq.takeWhile (fun (_, xSteps) -> xSteps <= maxYSteps)
        |> List.ofSeq
        |> List.map (fun (xVel, xSteps) ->
            // Here we're give that for a x velocity of xVel we'll hit in the
            // target in xSteps.
            // Now we need to find if there is a yVelocity that will hit between
            // y1-y2 in those steps. Can we do that arithmetically?
            //
            // We have the sum of series formula which will get us the y point
            // with a step size of -1:
            // so: sum = n/2(2a - (n-1))
            // * sum is the y-point we hit. We can set that to y1 and see how
            // close we get, and then what would happen with an extra steps
            // * a is the starting velocity we're trying to find
            // * n is the number of steps. That's fixed already
            //
            // so lets plug in what we know
            // y1 = (xSteps/2) * (2*a - xSteps + 1)
            // 0 = xSteps/2 * 2*a - (xSteps^2)/2 + xSteps/2 - y1 // simplify
            // 0 = xSteps * a - (xSteps^2)/2 + xSteps/2 - y1 // simplify
            // xSteps * a = (xSteps^2)/2 - xSteps/2 + y1 // rearrange
            // a = ((xSteps^2)/2 - xSteps/2 + y1)/xs // rearrange for x
            //
            // So we can find a - the initial y-velocity - knowing the steps and
            // the target we want to hit. Let's do that
            let xSteps = float xSteps
            let yVel = ((0.5 * xSteps * xSteps) - (0.5 * xSteps) + (float y1))/xSteps
            let yVel = floor yVel

            // now use sum of series to see where it would hit. It could miss,
            // or it could hit multiple spots in the target area with the same
            // number of steps, so try different velocities until we're
            // out of range.
            yVel
            |> List.unfold (fun yVel ->
                let y = (0.5*xSteps) * (2. * yVel - xSteps + 1.)
                if y < float y2 // we only want the case where y is going to be above or equal to y2
                then None
                else Some ((xVel, int yVel), yVel - 1.))) // -1, because we calculated the highest hit

        ]
        |> List.collect id |> List.collect id
        |> List.distinct

let input =
    readLinesWithHashComments "day17.txt"
    |> Seq.head
    |> String.splitIntoMatching "-?\d+"
    |> fun [x1;x2;y2;y1] -> (int x1,int y1),(int x2, int y2)

let part1 targetArea =
    targetArea
    |> calculateMaxY
    // Correct Answer: 13041 took: 0ms

let part2 targetArea =
    targetArea
    |> calculateXVelocityRange
    |> calculateTrajectories targetArea
    |> List.length
    // Correct Answer: 1031 took: 4ms

Helper.measurePart1 part1 input
Helper.measurePart2 part2 input
