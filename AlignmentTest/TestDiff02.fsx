#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

open System.Text.RegularExpressions


#load "..\Alignment\Alignment\AlignedList.fs"
#load "..\Alignment\Alignment\Extra\ExcelProviderHelper.fs"
open Alignment.AlignedList
open Alignment.Extra.ExcelProviderHelper

type WorkTable = 
    ExcelFile< @"G:\work\Projects\events2\EDM2 Site-List SK.xlsx",
               SheetName = "SITE_LIST",
               ForceString = true >

/// Note - we cannot implement interfaces (e.g. 'System.IComparable') for 
/// type abbreviations.
type WorkRow = WorkTable.Row



let getWorkRows () : WorkRow list = 
    let helper = 
        { new IExcelProviderHelper<WorkTable, WorkRow>
          with member __.ReadTableRows table = table.Data 
               member __.IsBlankRow row = match row.GetValue(0) with null -> true | _ -> false }

    excelReadRowsAsList helper (new WorkTable()) 
        


let normalizeDirName (str:string) : string = 
    str.Trim().Replace('_', '/')
    

let workRowKey (row:WorkRow) : string = row.``Site Common Name``.Trim()
let directoryKey (name:string) : string = normalizeDirName name




let getDirectoryName (line:string) : option<string> = 
    let regMatch = Regex.Match(line, @"^[d]\-{4}[ 0-9/:]*(?<dirname>([A-Z][A-Z0-9 _]*))")
    if regMatch.Success then 
        Some <| regMatch.Groups.["dirname"].Value.Trim()
    else 
        None

let getDirectoryNames (listingPath:string) : string list = 
    let text:string list = System.IO.File.ReadAllLines(listingPath) |> Array.toList
    text |> List.map getDirectoryName |> List.choose id


let getWorkItems () : AlignedList<string,WorkRow> = 
    getWorkRows () 
        |> AlignedList.ofList workRowKey

let getDirectories () : AlignedList<string,string> = 
    let ds1 = getDirectoryNames @"G:\work\Projects\events2\data\stws.txt"
    let ds2 = getDirectoryNames @"G:\work\Projects\events2\data\csos.txt"
    ds1 @ ds2 |> AlignedList.ofList directoryKey


let test01 () = 
    differenceL (getWorkItems ()) (getDirectories ())
        |> toList
        |> List.iter (fun row -> printfn "%s" row.``Site Common Name``) 


let test02 () = 
    intersection (getWorkItems ()) (getDirectories ())
        |> List.iter (fun (row,dir) -> printfn "%s: %s" row.``Site Common Name`` dir) 




