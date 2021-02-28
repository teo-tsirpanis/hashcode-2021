open System
open System.Collections
open System.Collections.Generic
open System.Collections.Immutable
open System.Diagnostics
open System.IO
open System.Linq
open System.Text
open System.Threading
open System.Threading.Tasks

[<Struct>]
type Intersection = Intersection of int

type Street = Street of name: string * latency: int * start: Intersection * ``end``: Intersection

type Car = Car of Street list

type ProblemInput = {
    SimulationDuration: int
    IntersectionCount: int
    CarBonus: int
    Streets: Street ImmutableArray
    Cars: Car ImmutableArray
}

type ScheduleEntry = ScheduleEntry of Street * duration: int

type ProblemOutput = ProblemOutput of ScheduleEntry list

let (|Int|_|) (x: string) =
    match Int32.TryParse x with
    | true, x -> Some x
    | false, _ -> None

let getNextLineSplitted (r: TextReader) =
    r.ReadLine().Split(' ', StringSplitOptions.RemoveEmptyEntries)

let readInput fileName =
    let streetLookup = Dictionary(StringComparer.Ordinal)
    use file = File.OpenText fileName
    match getNextLineSplitted file with
    | [|Int duration; Int intersectionCount; Int streetCount; Int carCount; Int bonus|] ->
        let streetsB = ResizeArray()
        for _ = 1 to streetCount do
            match getNextLineSplitted file with
            | [|Int intStart; Int intEnd; name; Int latency|] ->
                let street = Street(name, latency, Intersection intStart, Intersection intEnd)
                streetLookup.Add(name, street)
                streetsB.Add(street)
            | _ -> failwith "invalid input format"

        let usedStreets = HashSet()
        let carsB = ImmutableArray.CreateBuilder(carCount)
        for _ = 1 to carCount do
            getNextLineSplitted file
            |> Seq.skip 1
            |> Seq.map (fun x ->
                let street = streetLookup.[x]
                usedStreets.Add street |> ignore
                street)
            |> List.ofSeq
            |> Car
            |> carsB.Add

        {
            SimulationDuration = duration
            IntersectionCount = intersectionCount
            CarBonus = bonus
            Streets = ImmutableArray.CreateRange usedStreets
            Cars = carsB.MoveToImmutable()
        }
    | _ -> failwith "Invalid input format"

let writeOutput (fileName: string) (ProblemOutput scheduleEntries) =
    let entriesGroupped =
        scheduleEntries
        |> List.groupBy (fun (ScheduleEntry(Street(_, _, _, x), _)) -> x)
    use sw = new StreamWriter(fileName)
    sw.WriteLine entriesGroupped.Length
    for Intersection intersection, entries in entriesGroupped do
        sw.WriteLine intersection
        sw.WriteLine entries.Length
        for ScheduleEntry(Street(name, _, _, _), duration) in entries do
            sw.Write name
            sw.Write ' '
            sw.WriteLine duration

let theHolyAlgorithm input =
    let durationRoot = input.SimulationDuration |> float |> Math.Sqrt
    let carFrequency =
        let xs = Array.zeroCreate input.Streets.Length
        input.Cars
        |> Seq.collect (fun (Car x) -> x)
        |> Seq.countBy id
        |> readOnlyDict
    input.Streets
    |> Seq.groupBy (fun (Street(_, _, _, x)) -> x)
    |> Seq.collect (fun (_, streets) ->
        let streets = Array.ofSeq streets
        let totalFrequency = Array.sumBy (fun x -> carFrequency.[x]) streets
        streets
        |> Seq.map (fun street ->
            let duration =
                (float carFrequency.[street] / float totalFrequency) * durationRoot
                |> round
                |> int
                |> max 1
            ScheduleEntry(street, duration)))
    |> List.ofSeq
    |> ProblemOutput

let printfn fmt = Printf.kprintf Console.WriteLine fmt

[<EntryPoint>]
let main _ =
    Console.OutputEncoding <- Encoding.UTF8
    let inputFiles = Directory.GetFiles("./inputs/", "*.txt")
    inputFiles
    |> Array.Parallel.iter (fun inputFile ->
        let inputFileName = Path.GetFileName inputFile
        let input = readInput inputFile
        printfn "%s: Input read" inputFileName
        let sw = Stopwatch()
        sw.Start()
        let output = theHolyAlgorithm input
        sw.Stop()
        printfn "%s: The Holy Algorithm™ finished and took %O" inputFileName sw.Elapsed
        let outputFile = Path.Join("./outputs/", Path.ChangeExtension(inputFileName, ".out.txt"))
        writeOutput outputFile output
        printfn "%s: output written in %s" inputFileName outputFile
    )
    0
