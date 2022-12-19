module day19.BaseTypes

let s4 (l: int list) = $"{l[0]} {l[1]} {l[2]} {l[3]}"

type Resource =
    | Ore
    | Clay
    | Obsidian
    | Geode

type Production(production: int list) =
    member this.Value = production
    override this.ToString() = $"P([{s4 production}])"
    static member empty = Production [ 0; 0; 0; 0 ]
    static member oneOre = Production [ 1; 0; 0; 0 ]

type Resources(current: int list) =
    member this.Value = current
    member this.Geode = current[3]
    override this.ToString() = $"R([{s4 current}])"
