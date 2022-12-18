// For more information see https://aka.ms/fsharp-console-apps
open System.IO
open System 

let (|Int|_|) (s:string) =
    match Int64.TryParse s with
    | true,value -> Some value
    | false,_ -> None

let file = File.ReadLines "/tmp/aoc/input" |> Seq.toList

let parse (s:string) =
    s
    
let input = file |> List.map parse

input |> List.map (printfn "%A")