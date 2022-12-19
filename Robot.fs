module day19.Robot

open day19.BaseTypes

type Robot(resource: Resource, costs: Resources) =
    member this.Type = resource

    member this.Produces: Production =
        let ore = if resource = Ore then 1 else 0
        let clay = if resource = Clay then 1 else 0
        let obs = if resource = Obsidian then 1 else 0
        let geode = if resource = Geode then 1 else 0
        Production [ ore; clay; obs; geode ]

    member this.Costs = costs

    member this.CanBuild(resources: Resources) : bool =
        List.zip resources.Value costs.Value
        |> List.filter (fun (r, c) -> r < c)
        |> List.isEmpty

    member this.AddToProduction(production: Production) : Production =
        List.zip this.Produces.Value production.Value
        |> List.map (fun (a, b) -> a + b)
        |> Production

    override this.ToString() = $"Robot({resource} {costs})"

let buildRobot (robot: Robot) (current: Resources) : Production * Resources =
    let current =
        List.zip current.Value robot.Costs.Value
        |> List.map (fun (c, r) -> (c - r))
        |> Resources

    (robot.Produces, current)

type BluePrint(id: int, robots: Robot list) =
    member this.Geode : Robot = robots |> List.find (fun r -> r.Type = Geode)
    member this.Ore : Robot = robots |> List.find (fun r -> r.Type = Ore)
    member this.Obsidian : Robot = robots |> List.find (fun r -> r.Type = Obsidian)
    member this.Clay : Robot = robots |> List.find (fun r -> r.Type = Clay)
    member this.Robots = robots
    member this.Id = id

    override this.ToString() =
        $"BluePrint(id={id}: {robots[0]} {robots[1]} {robots[2]} {robots[3]})"
