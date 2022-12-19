module day19.Cache

open day19.BaseTypes

type Cache(best: int, bestCurrent: Map<int, Resources>, bestProduction: Map<int, Production>) =
    member this.Best = best
    member this.BestCurrent = bestCurrent
    member this.BestProduction = bestProduction

    member this.Register (time: int) (current: Resources) (production: Production) : Cache * bool =
        let best =
            if max best current.Geode > best then
                printfn $"BETTER: {current.Geode}"

            max best current.Geode

        if bestCurrent.ContainsKey time |> not then
            // printfn $"Register New Entry: C={s4 current} P={s4 production}"
            let bestCurrent = bestCurrent.Add(time, current)
            let bestProduction = bestProduction.Add(time, production)
            Cache(best, bestCurrent, bestProduction), true
        else
            let currZipped = List.zip current.Value bestCurrent[time].Value
            let prodZipped = List.zip production.Value bestProduction[time].Value

            let currentBetter: bool =
                currZipped |> List.tryFind (fun (c, b) -> (c < b)) |> Option.isNone

            let currentWorse: bool =
                currZipped |> List.tryFind (fun (c, b) -> (c > b)) |> Option.isNone

            let prodBetter: bool =
                prodZipped |> List.tryFind (fun (c, b) -> (c < b)) |> Option.isNone

            let prodWorse: bool =
                prodZipped |> List.tryFind (fun (c, b) -> (c > b)) |> Option.isNone

            if currentBetter && prodBetter then
                let bestCurrent = bestCurrent.Add(time, current)
                let bestProduction = bestProduction.Add(time, production)
                Cache(best, bestCurrent, bestProduction), true
            else
                let shouldContinue = (not currentWorse) || (not prodWorse)
                let cache = Cache(best, bestCurrent, bestProduction)
                cache, shouldContinue

    override this.ToString() = $"Cache({best})"
    static member empty =
        let startEmptyP = [ (0, [ 0; 0; 0; 0 ] |> Production) ] |> Map.ofList
        let startEmptyR = [ (0, [ 0; 0; 0; 0 ] |> Resources) ] |> Map.ofList
        Cache(0, startEmptyR, startEmptyP)
