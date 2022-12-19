open System.IO
open System

let (|Int|_|) (s: string) =
    match Int64.TryParse s with
    | true, value -> Some value
    | false, _ -> None

let addLists (a: int list) (b: int list) =
    List.zip a b |> List.map (fun (a, b) -> a + b)

let s4 (l: int list) = $"{l[0]} {l[1]} {l[2]} {l[3]}"

let file = File.ReadLines "/tmp/aoc/input.t" |> Seq.toList

type Resource =
    | Ore
    | Clay
    | Obsidian
    | Geode

type Robot(resource: Resource, costs: int list) =
    member this.Type = resource

    member this.Produces =
        let ore = if resource = Ore then 1 else 0
        let clay = if resource = Clay then 1 else 0
        let obs = if resource = Obsidian then 1 else 0
        let geode = if resource = Geode then 1 else 0
        [ ore; clay; obs; geode ]

    member this.Costs = costs

    member this.CanBuild(resources: int list) : bool =
        List.zip resources costs |> List.filter (fun (r, c) -> r < c) |> List.isEmpty

    member this.AddToProduction(resources: int list) =
        List.zip this.Produces resources |> List.map (fun (a, b) -> a + b)

    override this.ToString() =
        $"Robot({resource},prod={s4 this.Produces} costs={s4 costs}"

let parse (s: string) : (Robot list) =
    let orig = s
    let s = s.Split [| ' '; ':' |] |> Array.toList
    let blueprint = s[1] |> int
    let ore = s[7] |> int
    let oreRobot = Robot(Ore, [ ore; 0; 0; 0 ])
    let clayOre = s[13] |> int
    let clayRobot = Robot(Clay, [ clayOre; 0; 0; 0 ])
    let obsOre = s[19] |> int
    let obsClay = s[22] |> int
    let obsRobot = Robot(Obsidian, [ obsOre; obsClay; 0; 0 ])
    let geodeOre = s[28] |> int
    let geodeObs = s[31] |> int
    let geodeRobot = Robot(Geode, [ geodeOre; 0; geodeObs; 0 ])
    [ oreRobot; clayRobot; obsRobot; geodeRobot ]

let input = file |> List.map parse

// input |> List.map (printfn "%A")

let buildRobot (robot: Robot) (production: int list) (current: int list) =
    let current = List.zip current robot.Costs |> List.map (fun (c, r) -> (c - r))
    let production = addLists production robot.Produces
    (production, current)

type State(robots: Robot list, geodeRobot: Robot) =
    member this.Robots = robots
    member this.GeodeRobot = geodeRobot

    static member init(robots: Robot list) =
        let geode = robots |> List.filter (fun robot -> robot.Type = Geode)
        let robots = robots |> List.filter (fun robot -> robot.Type = Geode |> not)
        State(robots, geode[0])

type Cache(best: int, bestCurrent: Map<int, int list>, bestProduction: Map<int, int list>) =
    member this.Best = best
    member this.BestCurrent = bestCurrent
    member this.BestProduction = bestProduction

    member this.Register (time: int) (current: int list) (production: int list) : Cache * bool =
        let geode = current[3]

        let best =
            if max best geode > best then
                printfn $"BETTER: {geode}"

            max best geode

        if bestCurrent.ContainsKey time |> not then
            // printfn $"Register New Entry: C={s4 current} P={s4 production}"
            let bestCurrent = bestCurrent.Add(time, current)
            let bestProduction = bestProduction.Add(time, production)
            Cache(best, bestCurrent, bestProduction), true
        else
            let currZipped = List.zip current bestCurrent[time]
            let prodZipped = List.zip production bestProduction[time]

            let currentBetter: bool =
                currZipped |> List.tryFind (fun (c, b) -> (c < b)) |> Option.isNone

            let currentWorse: bool =
                currZipped |> List.tryFind (fun (c, b) -> (c > b)) |> Option.isNone

            let prodBetter: bool =
                prodZipped |> List.tryFind (fun (c, b) -> (c < b)) |> Option.isNone

            let prodWorse: bool =
                prodZipped |> List.tryFind (fun (c, b) -> (c > b)) |> Option.isNone
            // if current[3] > 0 then
            // printfn $"registering: {time} {s4 current} {s4 production} CB={currentBetter} CW={currentWorse} PB={prodBetter} PW={prodWorse}"
            if currentBetter && prodBetter then
                // printfn " replacing"
                let bestCurrent = bestCurrent.Add(time, current)
                let bestProduction = bestProduction.Add(time, production)
                Cache(best, bestCurrent, bestProduction), true
            else
                let shouldContinue = (not currentWorse) || (not prodWorse)
                //if not shouldContinue then
                //    printfn $" Recommending abort C={s4 current} P={s4 production} CB={s4 bestCurrent[time]} PB={s4 bestProduction[time]}"
                let cache = Cache(best, bestCurrent, bestProduction)
                cache, shouldContinue

    override this.ToString() = $"Cache({best})"

    static member empty =
        let startEmpty = [ (0, [ 0; 0; 0; 0 ]) ] |> Map.ofList
        Cache(0, startEmpty, startEmpty)

let canBuildEnough (time:int) (best:int) (curr:int) (geoprod:int) =
    let time = time - 1 // won't produce anything until next turn
    let rec fac n =
        if n > 7 then Int32.MaxValue
        elif n < 2 then 1
        else n * (fac (n-1))
    let potential = (geoprod * time) + curr + (fac time)
    let enough = potential > best 
    printfn $"canBuildEnough {time} {best} {curr} {geoprod} {potential} {enough}"
    enough

let rec step (i: int) (time: int) (state: State) (cache: Cache) (production: int list) (current: int list) : Cache =
    let i = i + 1
    let indent = " " |> String.replicate i
    let cache, shouldContinue = cache.Register time current production
    let canCatchUp = canBuildEnough time cache.Best current[3] production[3]
    printfn $"{indent}step[1]: {time} cache={cache} production={s4 production} current={s4 current}"

    if time < 1 || (not shouldContinue || (not canCatchUp)) then
        // printfn $"{indent}returning {current[3]}"
        cache
    else
        let current = addLists current production
        let time = time - 1
        // printfn $"{indent}step[2]: {time} production={s4 production} current={s4 current}"

        if state.GeodeRobot.CanBuild current then // always build geode
            // printfn "#### CAN BUILD GEODE ####"
            let production, current = buildRobot state.GeodeRobot production current
            step i time state cache production current
        else
            // printfn $"{indent}step[3]: production={s4 production} current={s4 current}"
            let canBuild = state.Robots |> List.filter (fun robot -> robot.CanBuild current)
            // printfn $"{indent}>canBuild: {canBuild.Length}"

            let rec tryBuilds (cache: Cache) (robots: Robot list) : Cache =
                match robots with
                | [] -> cache
                | robot :: robots ->
                    let production, current = buildRobot robot production current
                    let cache = step i time state cache production current
                    tryBuilds cache robots

            let cache: Cache = tryBuilds cache canBuild
            let noBuild = step i time state cache production current
            noBuild

step 0 24 (State.init (input[0])) Cache.empty [ 1; 0; 0; 0 ] [ 0; 0; 0; 0 ]
