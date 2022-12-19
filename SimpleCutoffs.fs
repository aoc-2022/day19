module day19.SimpleCutoffs

open day19.BaseTypes
open day19.Robot

type Cutoffs(bluePrint: BluePrint, maxMaterials: Resources) =
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
        
    static member init(bluePrint: BluePrint) =
        let costs = bluePrint.Robots |> List.map (fun robot -> robot.Costs.Value)
        let ore = costs |> List.map (fun c -> c[0]) |> List.max
        let clay = costs |> List.map (fun c -> c[0]) |> List.max
        let obsidian = costs |> List.map (fun c -> c[0]) |> List.max
        let maxMaterials = Resources [ ore; clay; obsidian; 0 ]
        Cutoffs(bluePrint, maxMaterials)
        
