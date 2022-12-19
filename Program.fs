open System.IO
open System
open day19.BaseTypes
open day19.Robot
open day19.Cache 

let (|Int|_|) (s: string) =
    match Int64.TryParse s with
    | true, value -> Some value
    | false, _ -> None

let addLists (a: int list) (b: int list) =
    List.zip a b |> List.map (fun (a, b) -> a + b)

let file = File.ReadLines "/tmp/aoc/input.t" |> Seq.toList

let parse (s: string) : BluePrint = 
    let s = s.Split [| ' '; ':' |] |> Array.toList
    let blueprint = s[1] |> int
    let ore = s[7] |> int
    let oreRobot = Robot(Ore, [ ore; 0; 0; 0 ] |> Resources)
    let clayOre = s[13] |> int
    let clayRobot = Robot(Clay, [ clayOre; 0; 0; 0 ] |> Resources)
    let obsOre = s[19] |> int
    let obsClay = s[22] |> int
    let obsRobot = Robot(Obsidian, [ obsOre; obsClay; 0; 0 ] |> Resources)
    let geodeOre = s[28] |> int
    let geodeObs = s[31] |> int
    let geodeRobot = Robot(Geode, [ geodeOre; 0; geodeObs; 0 ] |> Resources)
    BluePrint(blueprint, [ oreRobot; clayRobot; obsRobot; geodeRobot ] |> List.rev ) 

let input = file |> List.map parse

// input |> List.map (printfn "%A")

type State(robots: Robot list, geodeRobot: Robot) =
    member this.Robots = robots
    member this.GeodeRobot = geodeRobot

    static member init(robots: BluePrint) =
        let geode = robots.Robots |> List.filter (fun robot -> robot.Type = Geode)
        let robots = robots.Robots |> List.filter (fun robot -> robot.Type = Geode |> not)
        State(robots, geode[0])

let produce (current: Resources) (production: Production) : Resources =
    addLists current.Value production.Value |> Resources

let addFresh (production: Production) (fresh: Production) =
    addLists production.Value fresh.Value |> Production

let estimatedBuildTime (time: int) (production: Production) (robot: Robot) =
    let estimate ((prod, cost): int * int) = if prod + 1 >= cost then 1 else 2

    let costPairs: (int * int) list =
        List.zip production.Value robot.Costs.Value |> List.take 3

    costPairs |> List.map estimate |> List.max

let canBuildEnough (time: int) (best: int) (curr: int) (geoprod: int) (buildTime: int) =
    let rec fac (steps: int) n =
        if n > 7 then Int32.MaxValue
        elif n < 2 then 1
        else n * (fac steps (n - steps))

    let addedOld = fac 1 time
    let added = fac buildTime time
    let potential = (geoprod * time) + curr + added
    let potentialOld = (geoprod * time) + curr + addedOld
    let enough = potential > best
    enough

let rec step
    (i: int)
    (time: int)
    (state: State)
    (cache: Cache)
    (production: Production)
    (fresh: Production)
    (current: Resources)
    : Cache =
    let i = i + 1
    let time = time - 1
    let current = produce current production
    let production = addFresh production fresh
    let indent = " " |> String.replicate i
    let geoPerMin = estimatedBuildTime time production state.GeodeRobot

    if time < 3 then
        printfn
             $"{indent}minute {i}({time}: {cache} {production}  {current} {state.GeodeRobot} geoPerTime:{geoPerMin}"

    let cache, shouldContinue = cache.Register time current production

    let canCatchUp =
        canBuildEnough time cache.Best current.Value[3] production.Value[3] geoPerMin

    if time < 1 || (not shouldContinue || (not canCatchUp)) then
        cache
    else if state.GeodeRobot.CanBuild current then // always build geode
        let fresh, current = buildRobot state.GeodeRobot current
        step i time state cache production fresh current
    else
        let canBuild = state.Robots |> List.filter (fun robot -> robot.CanBuild current)

        let rec tryBuilds (cache: Cache) (robots: Robot list) : Cache =
            match robots with
            | [] -> cache
            | robot :: robots ->
                let fresh, current = buildRobot robot current
                let cache = step i time state cache production fresh current
                tryBuilds cache robots

        let cache: Cache = tryBuilds cache canBuild
        let noBuild = step i time state cache production Production.empty current
        noBuild

let solveForBluePrint (bluePrint: BluePrint) = 
    step
        0
        24
        (State.init bluePrint)
        Cache.empty
        (Production [ 1; 0; 0; 0 ])
        (Production [ 0; 0; 0; 0 ])
        (Resources [ 0; 0; 0; 0 ])

printfn $"{input[1]}"

solveForBluePrint (input[1])

// ORE=4  0 0
//CLAY=2  0 0
// OBS=3 14 0
// GEO=2  0 7 -> 2.0.7 + 1*2.0.7