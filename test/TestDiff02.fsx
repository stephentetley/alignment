// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

#r "netstandard"

open System.IO

#I @"C:\Users\stephen\.nuget\packages\ExcelProvider\1.0.1\lib\netstandard2.0"
#r "ExcelProvider.Runtime.dll"

#I @"C:\Users\stephen\.nuget\packages\ExcelProvider\1.0.1\typeproviders\fsharp41\netstandard2.0"
#r "ExcelDataReader.DataSet.dll"
#r "ExcelDataReader.dll"
#r "ExcelProvider.DesignTime.dll"
open FSharp.Interop.Excel

open System.Text.RegularExpressions


#load "..\src\SLAlignment\AlignedList.fs"
#load "..\src\SLAlignment\Extra\ExcelProviderHelper.fs"
open SLAlignment.AlignedList
open SLAlignment.Extra.ExcelProviderHelper

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




