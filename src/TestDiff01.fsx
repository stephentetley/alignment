#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

open System.Text.RegularExpressions

#load "Alignment/Common.fs"
#load @"Alignment/Set.fs"
open Alignment.Common
open Alignment.Set


type UploadTable = 
    ExcelFile< @"G:\work\Projects\rtu\Erskines\alignment\EDMS-final-docs-uploaded-june-2018.xlsx",
               SheetName = "Sheet1",
               ForceString = true >

/// Note - we cannot implement interfaces (e.g. 'System.IComparable') for 
/// type abbreviations.
type UploadRow = UploadTable.Row

let normalize (str:string) : string = str.Replace('_', ' ')

/// Note - it is a lot of work to get an ExcelFile<..>.Row to implement
/// System.IComparable, so we can put it in a Set<>.
[<CustomEquality; CustomComparison>]
[<Struct>]
type Upload = 
    Upload of UploadRow
        member v.unwrap = match v with | Upload x -> x
        override x.Equals(yobj) =
            match yobj with
            | :? Upload as y -> normalize x.unwrap.Title = normalize y.unwrap.Title
            | _ -> false

        override x.GetHashCode() = hash x.unwrap.Title

        interface System.IComparable with
            member x.CompareTo(yobj) = 
                match yobj with
                | :? Upload as y -> 
                    compare (normalize x.unwrap.Title) (normalize y.unwrap.Title)
                | _ -> invalidArg "yobj" "Cannot compare values of different types"



        



let uploadTableDict : ExcelProviderHelperDict<UploadTable, UploadRow> = 
    { GetRows     = fun sheet -> sheet.Data 
      NotNullProc = fun row -> match row.GetValue(0) with | null -> false | _ -> true }


let getSiteRows () : UploadRow list = 
    excelTableGetRows uploadTableDict (new UploadTable()) 
        |> Seq.toList 
        



let getDirectoryName (line:string) : option<string> = 
    let regMatch = Regex.Match(line, @"^[d]\-{4}[ 0-9/:]*(?<dirname>([A-Z][A-Z0-9 _]*))")
    if regMatch.Success then 
        Some <| regMatch.Groups.["dirname"].Value.Trim()
    else 
        None

let getDirectoryNames (listingPath:string) : string list = 
    let text:string list = System.IO.File.ReadAllLines(listingPath) |> Array.toList
    text |> List.map getDirectoryName |> List.choose id

let test01 ()  = 
    List.iter (printfn "%s") 
        <| getDirectoryNames @"G:\work\Projects\rtu\Erskines\alignment\dir.txt"
    List.iter (printfn "%A") 
        <| getSiteRows ()


let getUploads () : Set<Upload> = 
    getSiteRows () 
        |> List.map (fun x -> Upload(x))
        |> Set.ofList 

let getDirectories () : Set<string> = 
    getDirectoryNames @"G:\work\Projects\rtu\Erskines\alignment\dir.txt"
        |> Set.ofList


let testDiff01 () = 
    let compH : CompareH<string, Upload>  = 
        fun nameBad upload -> 
            let name = normalize nameBad
            let n = name.Length - 1
            let prefix = normalize <| upload.unwrap.Title.[0..n]
            printfn "compare: name'%s'; prefix='%s'" name prefix
            compare name prefix

            
    Set.iter (printfn "%s") 
        <| differenceL compH (getDirectories ()) (getUploads ())





