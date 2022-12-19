module day19.SimpleCutoffs

open day19.BaseTypes
open day19.Robot

type Cutoffs(bluePrint: BluePrint, maxMaterials: Resources, latestBeforeLeft:Map<Resource,int>) =
    member this.UsefulToBuild (time: Time) (robot: Robot) : bool =
        match robot.Type with
        | Geode -> time.Left > 0
        | Obsidian -> time.Left > 1
        | Clay -> time.Left > 2
        | Ore -> time.Left > 1

    member this.NeedMaterial (time: Time) (resources: Resources) (robot: Robot) : bool =
        match robot.Type with
        | Geode -> true
        | Obsidian -> (maxMaterials.Obsidian * (time.Left - 1)) > resources.Obsidian
        | Clay -> (maxMaterials.Clay * (time.Left - 2)) > resources.Clay
        | Ore -> (maxMaterials.Ore * (time.Left - 1)) > resources.Ore
    member this.StillTimeForRobot (time:Time) (production:Production) (robot:Robot) =
        if production.Clay = 0 && time.Left < latestBeforeLeft[Clay] then false
        elif production.Obsidian = 0 && time.Left < latestBeforeLeft[Obsidian] then false
        else true 
        
    static member init(bluePrint: BluePrint) =
        let costs = bluePrint.Robots |> List.map (fun robot -> robot.Costs.Value)
        let ore = costs |> List.map (fun c -> c[0]) |> List.max
        let clay = costs |> List.map (fun c -> c[0]) |> List.max
        let obsidian = costs |> List.map (fun c -> c[0]) |> List.max
        let maxMaterials = Resources [ ore; clay; obsidian; 0 ]
        
        let rec findNeeded t n =
            if n < 1 then t
            else findNeeded (t+1) (n-t)
        let obsidianTurns = maxMaterials.Obsidian |> findNeeded 0
        let obsidianTurns = obsidianTurns + 1
        let clayTurns = maxMaterials.Clay |> findNeeded 0
        let clayTurns = clayTurns + obsidianTurns + 1
        let latestBeforeLeft = [(Ore,0);(Clay,clayTurns);(Obsidian,obsidianTurns);(Geode,0)] |> Map.ofList

        printfn $"obsidianTurns: {obsidianTurns} clayTurns={clayTurns}" 
        Cutoffs(bluePrint, maxMaterials, latestBeforeLeft)
        
